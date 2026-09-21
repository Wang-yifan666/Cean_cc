namespace Cean

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

end Cean


/- ============================================================
   自测
   ============================================================ -/

open Cean

-- 关键字 / 标识符 / 数字 / 运算符 / 分隔符
#guard
  (tokenize "int x = 1 + 2 * 3;").toOption
    == some [.kwInt, .ident "x", .assign,
             .number 1, .plus, .number 2, .star, .number 3, .semi]

-- 所有单字符运算符
#guard
  (tokenize "- + * < ! ( ) { } ;").toOption
    == some [.minus, .plus, .star, .less, .bang,
             .lparen, .rparen, .lbrace, .rbrace, .semi]

-- 双字符运算符
#guard (tokenize "== &&").toOption == some [.eqeq, .andand]

-- 其余关键字
#guard
  (tokenize "if else while main").toOption
    == some [.kwIf, .kwElse, .kwWhile, .kwMain]

-- 行注释被跳过
#guard
  (tokenize "// 注释\nx = 1;").toOption
    == some [.ident "x", .assign, .number 1, .semi]

-- 块注释被跳过（含跨行）
#guard
  (tokenize "/* 注释\n   跨行 */ x").toOption
    == some [.ident "x"]

-- 下划线开头的标识符合法
#guard (tokenize "_x1").toOption == some [.ident "_x1"]

-- 非法字符报错
#guard (tokenize "x $ 1;").isOk == false

-- 块注释没有闭合时报错
#guard (tokenize "/* 没有闭合").isOk == false
