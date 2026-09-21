import Cean.AST
import Cean.Env

namespace Cean


def isTruthy (value : Int) : Bool :=
  value != 0


def boolToInt (value : Bool) : Int :=
  if value then 1 else 0


def evalExpr
    (env : Env) :
    Expr →
    Except String Int

  | .intLit n =>
      .ok n

  | .var name =>
      match Env.get env name with
      | some value =>
          .ok value
      | none =>
          .error s!"undefined variable: {name}"

  | .unary .logicalNot expr => do
      let value ← evalExpr env expr

      pure (
        if isTruthy value then
          0
        else
          1
      )

  | .binary .add lhs rhs => do
      let l ← evalExpr env lhs
      let r ← evalExpr env rhs
      pure (l + r)

  | .binary .sub lhs rhs => do
      let l ← evalExpr env lhs
      let r ← evalExpr env rhs
      pure (l - r)

  | .binary .mul lhs rhs => do
      let l ← evalExpr env lhs
      let r ← evalExpr env rhs
      pure (l * r)

  | .binary .lt lhs rhs => do
      let l ← evalExpr env lhs
      let r ← evalExpr env rhs

      pure (
        boolToInt (decide (l < r))
      )

  | .binary .eq lhs rhs => do
      let l ← evalExpr env lhs
      let r ← evalExpr env rhs

      pure (
        boolToInt (l == r)
      )

  | .binary .logicalAnd lhs rhs => do
      /-
      必须 short-circuit。

      0 && undefined_variable

      不允许求值 RHS。
      -/
      let l ← evalExpr env lhs

      if isTruthy l then
        let r ← evalExpr env rhs
        pure (boolToInt (isTruthy r))
      else
        pure 0


end Cean


-- Test
open Cean

private def testEnv : Env :=
  Env.set
    (Env.set Env.empty "x" 3)
    "y"
    4


#guard
  (evalExpr testEnv
    (.binary .add
      (.intLit 1)
      (.binary .mul
        (.intLit 2)
        (.intLit 3)))).toOption
  ==
  some 7


#guard
  (evalExpr testEnv
    (.binary .sub
      (.intLit 1)
      (.intLit 4))).toOption
  ==
  some (-3)


#guard
  (evalExpr testEnv (.var "x")).toOption
    ==
  some 3


#guard
  (evalExpr testEnv (.var "z")).isOk
    ==
  false


#guard
  (evalExpr testEnv
    (.binary .lt
      (.var "x")
      (.var "y"))).toOption
  ==
  some 1


#guard
  (evalExpr testEnv
    (.binary .eq
      (.var "x")
      (.var "y"))).toOption
  ==
  some 0


-- !0 == 1
#guard
  (evalExpr testEnv
    (.unary .logicalNot
      (.intLit 0))).toOption
  ==
  some 1


-- !42 == 0
#guard
  (evalExpr testEnv
    (.unary .logicalNot
      (.intLit 42))).toOption
  ==
  some 0


-- C: (1 < 2) + 10 == 11
#guard
  (evalExpr testEnv
    (.binary .add
      (.binary .lt
        (.intLit 1)
        (.intLit 2))
      (.intLit 10))).toOption
  ==
  some 11


-- false && RHS，RHS 不应执行
#guard
  (evalExpr testEnv
    (.binary .logicalAnd
      (.binary .lt
        (.intLit 1)
        (.intLit 0))
      (.binary .eq
        (.var "undefined")
        (.intLit 0)))).toOption
  ==
  some 0


-- true && RHS，RHS 会执行，因此报错
#guard
  (evalExpr testEnv
    (.binary .logicalAnd
      (.binary .lt
        (.intLit 0)
        (.intLit 1))
      (.binary .eq
        (.var "undefined")
        (.intLit 0)))).isOk
  ==
  false
