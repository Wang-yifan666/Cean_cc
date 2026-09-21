import Cean.AST

namespace Cean

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

end Cean


/- ============================================================
   自测
   ============================================================ -/

open Cean

-- 空列表折叠成 skip
#guard Stmt.seqMany [] == Stmt.skip

-- 两条语句折叠成右结合的嵌套 seq
#guard
  Stmt.seqMany [.skip, .skip]
    == Stmt.seq .skip (Stmt.seq .skip .skip)

-- declaredNames 收集所有声明点，赋值不算
#guard (Stmt.assign "x" (.const 1)).declaredNames == []
#guard
  (Stmt.seq (.decl "a" (.const 1)) (.decl "b" (.const 2))).declaredNames
    == ["a", "b"]

-- 两个分支里的声明都会被收集
#guard
  (Stmt.ifThenElse (.eq (.const 0) (.const 0))
    (.decl "t" (.const 1)) (.decl "e" (.const 2))).declaredNames
    == ["t", "e"]

-- 循环体里的声明会被收集
#guard
  (Stmt.while (.less (.const 0) (.const 1)) (.decl "i" (.const 0))).declaredNames
    == ["i"]
