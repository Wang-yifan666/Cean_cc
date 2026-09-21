namespace Cean

inductive UnaryOp where
  | logicalNot
deriving Repr, BEq


inductive BinaryOp where
  | add
  | sub
  | mul

  | lt
  | eq

  | logicalAnd
deriving Repr, BEq


inductive Expr where
  | intLit : Int → Expr
  | var    : String → Expr

  | unary :
      UnaryOp →
      Expr →
      Expr

  | binary :
      BinaryOp →
      Expr →
      Expr →
      Expr

deriving Repr, BEq

end Cean


-- Test
open Cean

#guard
  Expr.binary .add (.intLit 1) (.intLit 2)
    ==
  Expr.binary .add (.intLit 1) (.intLit 2)

#guard
  Expr.binary .add (.intLit 1) (.intLit 2)
    !=
  Expr.binary .add (.intLit 2) (.intLit 1)

#guard
  Expr.binary .eq (.intLit 1) (.intLit 1)
    !=
  Expr.unary .logicalNot
    (Expr.binary .eq (.intLit 1) (.intLit 1))

#guard
  Expr.binary .logicalAnd
    (Expr.binary .lt (.intLit 1) (.intLit 2))
    (Expr.unary .logicalNot
      (Expr.binary .eq (.var "x") (.intLit 0)))
  ==
  Expr.binary .logicalAnd
    (Expr.binary .lt (.intLit 1) (.intLit 2))
    (Expr.unary .logicalNot
      (Expr.binary .eq (.var "x") (.intLit 0)))
