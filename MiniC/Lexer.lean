namespace MiniC

inductive Token where
  | kwInt
  | kwIf
  | kwElse
  | kwWhile
  | kwMain

  | ident  : String → Token
  | number : Int → Token

  | plus
  | minus
  | star

  | assign
  | eqeq
  | less
  | andand
  | bang

  | lparen
  | rparen
  | lbrace
  | rbrace
  | semi

deriving Repr, BEq


private def isIdentStart (c : Char) : Bool :=
  c.isAlpha || c == '_'


private def isIdentChar (c : Char) : Bool :=
  c.isAlphanum || c == '_'


private def spanChars
    (p : Char → Bool) :
    List Char →
    List Char × List Char

  | [] =>
      ([], [])

  | c :: cs =>
      if p c then
        let (taken, rest) := spanChars p cs
        (c :: taken, rest)
      else
        ([], c :: cs)


private def wordToken (word : String) : Token :=
  match word with
  | "int" =>
      .kwInt

  | "if" =>
      .kwIf

  | "else" =>
      .kwElse

  | "while" =>
      .kwWhile

  | "main" =>
      .kwMain

  | name =>
      .ident name


private def dropLineComment :
    List Char → List Char

  | [] =>
      []

  | '\n' :: rest =>
      rest

  | _ :: rest =>
      dropLineComment rest


private def dropBlockComment :
    List Char →
    Except String (List Char)

  | [] =>
      .error "unterminated block comment"

  | '*' :: '/' :: rest =>
      .ok rest

  | _ :: rest =>
      dropBlockComment rest


private partial def lexChars :
    List Char →
    Except String (List Token)

  | [] =>
      .ok []

  | c :: cs => do

      if c.isWhitespace then
        lexChars cs

      else if c.isDigit then

        let (digits, rest) :=
          spanChars
            (fun ch => ch.isDigit)
            cs

        let text :=
          String.ofList (c :: digits)

        match text.toInt? with

        | some value =>
            let tokens ← lexChars rest
            pure (.number value :: tokens)

        | none =>
            .error s!"invalid integer: {text}"

      else if isIdentStart c then

        let (chars, rest) :=
          spanChars isIdentChar cs

        let word :=
          String.ofList (c :: chars)

        let tokens ← lexChars rest

        pure (
          wordToken word :: tokens
        )

      else
        match c, cs with

        | '/', '/' :: rest =>
            lexChars (dropLineComment rest)

        | '/', '*' :: rest => do
            let rest ← dropBlockComment rest
            lexChars rest

        | '=', '=' :: rest => do
            let tokens ← lexChars rest
            pure (.eqeq :: tokens)

        | '&', '&' :: rest => do
            let tokens ← lexChars rest
            pure (.andand :: tokens)

        | '+', rest => do
            let tokens ← lexChars rest
            pure (.plus :: tokens)

        | '-', rest => do
            let tokens ← lexChars rest
            pure (.minus :: tokens)

        | '*', rest => do
            let tokens ← lexChars rest
            pure (.star :: tokens)

        | '=', rest => do
            let tokens ← lexChars rest
            pure (.assign :: tokens)

        | '<', rest => do
            let tokens ← lexChars rest
            pure (.less :: tokens)

        | '!', rest => do
            let tokens ← lexChars rest
            pure (.bang :: tokens)

        | '(', rest => do
            let tokens ← lexChars rest
            pure (.lparen :: tokens)

        | ')', rest => do
            let tokens ← lexChars rest
            pure (.rparen :: tokens)

        | '{', rest => do
            let tokens ← lexChars rest
            pure (.lbrace :: tokens)

        | '}', rest => do
            let tokens ← lexChars rest
            pure (.rbrace :: tokens)

        | ';', rest => do
            let tokens ← lexChars rest
            pure (.semi :: tokens)

        | _, _ =>
            .error
              s!"unexpected character: {reprStr c}"


def tokenize
    (source : String) :
    Except String (List Token) :=
  lexChars source.toList

end MiniC


-- Test
open MiniC
#eval tokenize "int x = 1 + 2 * 3;"
