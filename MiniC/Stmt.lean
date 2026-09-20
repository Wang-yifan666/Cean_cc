import MiniC.AST

namespace MiniC

-- 通过递归定义语句
inductive Stmt where
  | skip
  | decl
      (name : String)
      (init : AExpr)

  | assign
      (name : String)
      (value : AExpr)

  | seq
      (first : Stmt)
      (second : Stmt)

  | ifThenElse
      (cond : BExpr)
      (thenBranch : Stmt)
      (elseBranch : Stmt)

  | while
      (cond : BExpr)
      (body : Stmt)

deriving Repr, BEq

-- 把语句列表转换为一个嵌套的语句
def Stmt.seqMany : List Stmt → Stmt
  | [] =>
      .skip

  | stmt :: rest =>
      .seq stmt (Stmt.seqMany rest)

def Stmt.declaredNames : Stmt → List String

  | .skip =>
      []

  | .decl name _ =>
      [name]

  | .assign _ _ =>
      []

  | .seq first second =>
      first.declaredNames
        ++ second.declaredNames

  | .ifThenElse _ thenBranch elseBranch =>
      thenBranch.declaredNames
        ++ elseBranch.declaredNames

  | .while _ body =>
      body.declaredNames

end MiniC
