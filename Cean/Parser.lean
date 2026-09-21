import Init.Control.State

import Cean.Lexer
import Cean.AST
import Cean.Stmt

namespace Cean

/-
Parser α 的本质：
List Token -> Except String (α × List Token)
使用 StateT 来隐藏剩余 Token 的传递，让 do 记法自动处理状态。
-/
abbrev Parser (α : Type) :=
  StateT (List Token) (Except String) α


/- 查看当前 Token，但不消耗 -/
private def peekToken : Parser (Option Token) := do
  let tokens ← StateT.get
  match tokens with
  | [] => pure none
  | token :: _ => pure (some token)


/- 取出一个 Token，并推进输入流 -/
private def nextToken : Parser Token := do
  let tokens ← StateT.get
  match tokens with
  | [] => throw "unexpected end of file"
  | token :: rest =>
      StateT.set rest
      pure token


/- 要求下一个 Token 必须是 wanted -/
private def expect (wanted : Token) : Parser Unit := do
  let token ← nextToken
  if token == wanted then
    pure ()
  else
    throw s!"\nexpected: {reprStr wanted}\ngot:      {reprStr token}"


/- ============================================================
   Arithmetic Expressions
   ============================================================ -/

mutual

  partial def parseAExpr : Parser AExpr :=
    parseAddSub


  /- primary: 123 | x | (expression) -/
  partial def parsePrimary : Parser AExpr := do
    let token ← nextToken
    match token with
    | .number value => pure (.const value)
    | .ident name   => pure (.var name)
    | .lparen =>
        let expr ← parseAExpr
        expect .rparen
        pure expr
    | other =>
        throw s!"expected arithmetic expression, got {reprStr other}"


  /- multiplication: a * b * c (优先级高于 + -) -/
  partial def parseMul : Parser AExpr := do
    let lhs ← parsePrimary
    parseMulTail lhs


  partial def parseMulTail (lhs : AExpr) : Parser AExpr := do
    let token ← peekToken
    match token with
    | some .star =>
        let _ ← nextToken
        let rhs ← parsePrimary
        parseMulTail (.mul lhs rhs)
    | _ => pure lhs


  /- addition / subtraction: a + b - c -/
  partial def parseAddSub : Parser AExpr := do
    let lhs ← parseMul
    parseAddSubTail lhs


  partial def parseAddSubTail (lhs : AExpr) : Parser AExpr := do
    let token ← peekToken
    match token with
    | some .plus =>
        let _ ← nextToken
        let rhs ← parseMul
        parseAddSubTail (.add lhs rhs)
    | some .minus =>
        let _ ← nextToken
        let rhs ← parseMul
        parseAddSubTail (.sub lhs rhs)
    | _ => pure lhs

end


/- ============================================================
   Boolean Expressions
   ============================================================ -/

/- comparison: x < 10 | x == y -/
partial def parseComparison : Parser BExpr := do
  let lhs ← parseAExpr
  let op ← nextToken
  match op with
  | .less =>
      let rhs ← parseAExpr
      pure (.less lhs rhs)
  | .eqeq =>
      let rhs ← parseAExpr
      pure (.eq lhs rhs)
  | other =>
      throw s!"\nexpected comparison operator (< or ==),\ngot {reprStr other}"


/- ! 例如: !x < 10 会解释成 !(x < 10) -/
partial def parseNot : Parser BExpr := do
  let token ← peekToken
  match token with
  | some .bang =>
      let _ ← nextToken
      let expr ← parseNot
      pure (.not expr)
  | _ => parseComparison


/- && -/
mutual

  partial def parseAnd : Parser BExpr := do
    let lhs ← parseNot
    parseAndTail lhs


  partial def parseAndTail (lhs : BExpr) : Parser BExpr := do
    let token ← peekToken
    match token with
    | some .andand =>
        let _ ← nextToken
        let rhs ← parseNot
        parseAndTail (.and lhs rhs)
    | _ => pure lhs

end


def parseBExpr : Parser BExpr :=
  parseAnd


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
        let init ← parseAExpr
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
        let rhs ← parseAExpr
        expect .semi
        pure (.assign name rhs)

    /- if (...) { ... } else { ... } -/
    | some .kwIf =>
        let _ ← nextToken
        expect .lparen
        let cond ← parseBExpr
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
        let cond ← parseBExpr
        expect .rparen
        let body ← parseBlock
        pure (.while cond body)

    /- { ... } -/
    | some .lbrace =>
        parseBlock

    /- ; -/
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
    parseStmtList []


  partial def parseStmtList (acc : List Stmt) : Parser Stmt := do
    let token ← peekToken
    match token with
    /- block 结束 -/
    | some .rbrace =>
        let _ ← nextToken
        pure (Stmt.seqMany acc.reverse)
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
  /- main 后面不应该再有 Token -/
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


/- ============================================================
   自测
   ============================================================ -/

open Cean

-- 注意：块内的语句列表会被 seqMany 收尾，末尾一定带一个 skip

-- 乘法优先级高于加法：1 + 2 * 3  解析成  1 + (2 * 3)
#guard
  (parseSource "int main() { int x = 1 + 2 * 3; }").toOption
    == some (Stmt.seqMany [
         .decl "x" (.add (.const 1) (.mul (.const 2) (.const 3)))])

-- 括号可以覆盖优先级：(1 + 2) * 3
#guard
  (parseSource "int main() { int x = (1 + 2) * 3; }").toOption
    == some (Stmt.seqMany [
         .decl "x" (.mul (.add (.const 1) (.const 2)) (.const 3))])

-- 加减法左结合：1 - 2 + 3  解析成  (1 - 2) + 3
#guard
  (parseSource "int main() { int x = 1 - 2 + 3; }").toOption
    == some (Stmt.seqMany [
         .decl "x" (.add (.sub (.const 1) (.const 2)) (.const 3))])

-- 空程序
#guard (parseSource "int main() { }").toOption == some .skip

-- 注释被跳过
#guard
  (parseSource "int main() { /* 块注释 */ int x = 1; // 行注释\n}").toOption
    == some (Stmt.seqMany [.decl "x" (.const 1)])

-- while 语句（循环体同样被 seqMany 收尾）
#guard
  (parseSource "int main() { while (x < 1) { x = x + 1; } }").toOption
    == some (Stmt.seqMany [
         .while (.less (.var "x") (.const 1))
                (Stmt.seqMany [.assign "x" (.add (.var "x") (.const 1))])])

-- if / else 语句
#guard
  (parseSource "int main() { if (x == 1) { x = 2; } else { x = 3; } }").toOption
    == some (Stmt.seqMany [
         .ifThenElse (.eq (.var "x") (.const 1))
                     (Stmt.seqMany [.assign "x" (.const 2)])
                     (Stmt.seqMany [.assign "x" (.const 3)])])

-- 没有 else 时补一个 skip
#guard
  (parseSource "int main() { if (x == 1) { x = 2; } }").toOption
    == some (Stmt.seqMany [
         .ifThenElse (.eq (.var "x") (.const 1))
                     (Stmt.seqMany [.assign "x" (.const 2)]) .skip])

-- 多个语句按书写顺序串联
#guard
  (parseSource "int main() { int a = 1; int b = 2; }").toOption
    == some (Stmt.seqMany [.decl "a" (.const 1), .decl "b" (.const 2)])

-- 缺少分号要报错
#guard (parseSource "int main() { int x = 1 }").isOk == false

-- 缺少 main 要报错
#guard (parseSource "int x = 1;").isOk == false

-- main 之后还有内容要报错
#guard (parseSource "int main() { } int x = 1;").isOk == false

-- 未闭合的花括号要报错
#guard (parseSource "int main() { int x = 1;").isOk == false
