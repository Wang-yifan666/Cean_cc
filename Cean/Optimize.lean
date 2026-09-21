import Cean.AST

namespace Cean

def optimize : AExpr → AExpr
  | .const n =>
      .const n

  | .var name =>
      .var name

  | .add lhs rhs =>
      let lhs' := optimize lhs
      let rhs' := optimize rhs

      match lhs', rhs' with
      | .const a, .const b =>
          .const (a + b)

      | e, .const 0 =>
          e

      | .const 0, e =>
          e

      | l, r =>
          .add l r

  | .sub lhs rhs =>
      let lhs' := optimize lhs
      let rhs' := optimize rhs

      match lhs', rhs' with
      | .const a, .const b =>
          .const (a - b)

      | e, .const 0 =>
          e

      | l, r =>
          .sub l r

  | .mul lhs rhs =>
      let lhs' := optimize lhs
      let rhs' := optimize rhs

      match lhs', rhs' with
      | .const a, .const b =>
          .const (a * b)

      | e, .const 1 =>
          e

      | .const 1, e =>
          e

      | l, r =>
          .mul l r

end Cean
