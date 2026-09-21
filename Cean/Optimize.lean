import Cean.AST

namespace Cean


private def truthy (value : Int) : Bool :=
  value != 0


private def boolToInt (value : Bool) : Int :=
  if value then 1 else 0


def optimize : Expr → Expr

  | .intLit n =>
      .intLit n

  | .var name =>
      .var name


  | .unary .logicalNot expr =>

      let expr' :=
        optimize expr

      match expr' with

      | .intLit n =>
          .intLit (
            if truthy n then 0 else 1
          )

      | other =>
          .unary .logicalNot other


  | .binary op lhs rhs =>

      let lhs' :=
        optimize lhs

      let rhs' :=
        optimize rhs

      match op with

      | .add =>
          match lhs', rhs' with

          | .intLit a, .intLit b =>
              .intLit (a + b)

          | e, .intLit 0 =>
              e

          | .intLit 0, e =>
              e

          | l, r =>
              .binary .add l r


      | .sub =>
          match lhs', rhs' with

          | .intLit a, .intLit b =>
              .intLit (a - b)

          | e, .intLit 0 =>
              e

          | l, r =>
              .binary .sub l r


      | .mul =>
          match lhs', rhs' with

          | .intLit a, .intLit b =>
              .intLit (a * b)

          | e, .intLit 1 =>
              e

          | .intLit 1, e =>
              e

          | l, r =>
              .binary .mul l r


      | .lt =>
          match lhs', rhs' with

          | .intLit a, .intLit b =>
              .intLit (
                boolToInt (decide (a < b))
              )

          | l, r =>
              .binary .lt l r


      | .eq =>
          match lhs', rhs' with

          | .intLit a, .intLit b =>
              .intLit (
                boolToInt (a == b)
              )

          | l, r =>
              .binary .eq l r


      | .logicalAnd =>
          match lhs', rhs' with

          | .intLit a, .intLit b =>
              .intLit (
                boolToInt (
                  truthy a && truthy b
                )
              )

          | l, r =>
              .binary .logicalAnd l r


end Cean


open Cean


#guard
  optimize
    (.binary .add
      (.intLit 1)
      (.intLit 2))
  ==
  .intLit 3


#guard
  optimize
    (.binary .add
      (.binary .lt
        (.intLit 1)
        (.intLit 2))
      (.intLit 10))
  ==
  .intLit 11


#guard
  optimize
    (.unary .logicalNot
      (.intLit 0))
  ==
  .intLit 1
