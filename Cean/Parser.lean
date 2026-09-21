import Cean.Lexer
import Cean.AST
import Cean.Stmt

namespace Cean

abbrev Parser (α : Type) :=
  StateT (List Token) (Except String) α

private def peekToken : Parser (Option Token) := do
  let tokens ← StateT.get
  match tokens with
  | [] => pure none
  | token :: _ => pure (some token)

private def nextToken : Parser Token := do
  let tokens ← StateT.get
  match tokens with
  | [] => throw "unexpected end of file"
  | token :: rest =>
      StateT.set rest
      pure token

private def expect (wanted : Token) : Parser Unit := do
  let token ← nextToken
  if token == wanted then
    pure ()
  else
    throw s!"\nexpected: {reprStr wanted}\ngot:      {reprStr token}"

/- ============================================================
   Expressions

   precedence, low → high:

   &&
   ==
   <
   + -
   *
   !
   primary
   ============================================================ -/

mutual

  partial def parseExpr : Parser Expr :=
    parseLogicalAnd

  /- && -/
  partial def parseLogicalAnd : Parser Expr := do
    let lhs ← parseEquality
    parseLogicalAndTail lhs

  partial def parseLogicalAndTail (lhs : Expr) : Parser Expr := do
    let token ← peekToken
    match token with
    | some .andand =>
        let _ ← nextToken
        let rhs ← parseEquality
        parseLogicalAndTail (.binary .logicalAnd lhs rhs)
    | _ => pure lhs

  /- == -/
  partial def parseEquality : Parser Expr := do
    let lhs ← parseRelational
    parseEqualityTail lhs

  partial def parseEqualityTail (lhs : Expr) : Parser Expr := do
    let token ← peekToken
    match token with
    | some .eqeq =>
        let _ ← nextToken
        let rhs ← parseRelational
        parseEqualityTail (.binary .eq lhs rhs)
    | _ => pure lhs

  /- < -/
  partial def parseRelational : Parser Expr := do
    let lhs ← parseAddSub
    parseRelationalTail lhs

  partial def parseRelationalTail (lhs : Expr) : Parser Expr := do
    let token ← peekToken
    match token with
    | some .less =>
        let _ ← nextToken
        let rhs ← parseAddSub
        parseRelationalTail (.binary .lt lhs rhs)
    | _ => pure lhs

  /- + / - -/
  partial def parseAddSub : Parser Expr := do
    let lhs ← parseMul
    parseAddSubTail lhs

  partial def parseAddSubTail (lhs : Expr) : Parser Expr := do
    let token ← peekToken
    match token with
    | some .plus =>
        let _ ← nextToken
        let rhs ← parseMul
        parseAddSubTail (.binary .add lhs rhs)
    | some .minus =>
        let _ ← nextToken
        let rhs ← parseMul
        parseAddSubTail (.binary .sub lhs rhs)
    | _ => pure lhs

  /- * -/
  partial def parseMul : Parser Expr := do
    let lhs ← parseUnary
    parseMulTail lhs

  partial def parseMulTail (lhs : Expr) : Parser Expr := do
    let token ← peekToken
    match token with
    | some .star =>
        let _ ← nextToken
        let rhs ← parseUnary
        parseMulTail (.binary .mul lhs rhs)
    | _ => pure lhs

  /- ! -/
  partial def parseUnary : Parser Expr := do
    let token ← peekToken
    match token with
    | some .bang =>
        let _ ← nextToken
        let expr ← parseUnary
        pure (.unary .logicalNot expr)
    | _ =>
        parsePrimary

  /- number | identifier | (expr) -/
  partial def parsePrimary : Parser Expr := do
    let token ← nextToken
    match token with
    | .number value => pure (.intLit value)
    | .ident name   => pure (.var name)
    | .lparen =>
        let expr ← parseExpr
        expect .rparen
        pure expr
    | other =>
        throw s!"expected expression, got {reprStr other}"

end

/- ============================================================
   Statements
   ============================================================ -/

mutual

  partial def parseStmt : Parser Stmt := do
    let token ← peekToken
    match token with

    /- int x = expr; -/
    | some .kwInt =>
        let _ ← nextToken
        let nameToken ← nextToken
        let name ←
          match nameToken with
          | .ident name => pure name
          | other => throw s!"expected variable name, got {reprStr other}"
        expect .assign
        let init ← parseExpr
        expect .semi
        pure (.decl name init)

    /- x = expr; -/
    | some (.ident _) =>
        let nameToken ← nextToken
        let name ←
          match nameToken with
          | .ident name => pure name
          | _ => throw "internal parser error"
        expect .assign
        let rhs ← parseExpr
        expect .semi
        pure (.assign name rhs)

    /- if (...) { ... } else { ... } -/
    | some .kwIf =>
        let _ ← nextToken
        expect .lparen
        let cond ← parseExpr
        expect .rparen
        let thenBranch ← parseBlock
        let next ← peekToken
        match next with
        | some .kwElse =>
            let _ ← nextToken
            let elseBranch ← parseBlock
            pure (.ifThenElse cond thenBranch elseBranch)
        | _ =>
            pure (.ifThenElse cond thenBranch .skip)

    /- while (...) { ... } -/
    | some .kwWhile =>
        let _ ← nextToken
        expect .lparen
        let cond ← parseExpr
        expect .rparen
        let body ← parseBlock
        pure (.while cond body)

    /- standalone block -/
    | some .lbrace =>
        parseBlock

    /- empty statement -/
    | some .semi =>
        let _ ← nextToken
        pure .skip

    | some other =>
        throw s!"unexpected token at statement: {reprStr other}"

    | none =>
        throw "unexpected end of file while parsing statement"

  /- { stmt stmt stmt } -/
  partial def parseBlock : Parser Stmt := do
    expect .lbrace
    let stmts ← parseStmtList []
    pure (.block stmts)

  partial def parseStmtList (acc : List Stmt) : Parser (List Stmt) := do
    let token ← peekToken
    match token with
    | some .rbrace =>
        let _ ← nextToken
        pure acc.reverse
    | none =>
        throw "unexpected end of file inside block"
    | _ =>
        let stmt ← parseStmt
        parseStmtList (stmt :: acc)

end

/- ============================================================
   Program
   ============================================================ -/

/- 目前只支持：int main() { ... } -/
def parseProgram : Parser Stmt := do
  expect .kwInt
  expect .kwMain
  expect .lparen
  expect .rparen
  let body ← parseBlock
  let remaining ← StateT.get
  match remaining with
  | [] => pure body
  | token :: _ =>
      throw s!"unexpected token after main: {reprStr token}"

/- String -> Lexer -> Token -> Parser -> AST -/
def parseSource (source : String) : Except String Stmt := do
  let tokens ← tokenize source
  parseProgram.run' tokens

end Cean

-- Test
open Cean

#guard
  (parseSource
    "int main() {
       int x = 1;

       {
         int y = 2;
       }

       x = 3;
     }").toOption
  ==
  some (
    .block [
      .decl "x"
        (.intLit 1),

      .block [
        .decl "y"
          (.intLit 2)
      ],

      .assign "x"
        (.intLit 3)
    ]
  )
