import Cean.AST

namespace Cean

inductive Stmt where
  | skip

  | decl
      (name : String)
      (init : Expr)

  | assign
      (name : String)
      (value : Expr)

  /--
  Lexical block.

  保留源代码中的 `{ ... }` 边界。
  以后 scope / lifetime / shadowing 都依赖它。
  -/
  | block
      (stmts : List Stmt)

  | ifThenElse
      (cond : Expr)
      (thenBranch : Stmt)
      (elseBranch : Stmt)

  | while
      (cond : Expr)
      (body : Stmt)

deriving Repr, BEq


/- ============================================================
   Declared variables
   ============================================================ -/

/-
Stmt 是 nested inductive type：

    Stmt.block : List Stmt → Stmt

所以这里用 mutual recursion 同时遍历 Stmt 和 List Stmt。
-/
mutual

  def Stmt.declaredNames : Stmt → List String

    | .skip =>
        []

    | .decl name _ =>
        [name]

    | .assign _ _ =>
        []

    | .block stmts =>
        Stmt.declaredNamesList stmts

    | .ifThenElse _ thenBranch elseBranch =>
        thenBranch.declaredNames
          ++ elseBranch.declaredNames

    | .while _ body =>
        body.declaredNames


  def Stmt.declaredNamesList : List Stmt → List String

    | [] =>
        []

    | stmt :: rest =>
        stmt.declaredNames
          ++ Stmt.declaredNamesList rest

end


end Cean


/- ============================================================
   Tests
   ============================================================ -/

open Cean


#guard
  (Stmt.block []).declaredNames
    ==
  []


#guard
  (Stmt.block [
    .decl "a" (.intLit 1),
    .decl "b" (.intLit 2)
  ]).declaredNames
    ==
  ["a", "b"]


-- 最重要的测试：nested block 必须保留下来
#guard
  (Stmt.block [
    .decl "a" (.intLit 1),

    .block [
      .decl "b" (.intLit 2)
    ]
  ]).declaredNames
    ==
  ["a", "b"]

#check Expr
#guard
  (Stmt.ifThenElse
    (.binary .eq
      (.intLit 0)
      (.intLit 0))
    (.block [
      .decl "t" (.intLit 1)
    ])
    (.block [
      .decl "e" (.intLit 2)
    ])).declaredNames
    ==
  ["t", "e"]
