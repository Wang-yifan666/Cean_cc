import Cean.AST
import Cean.Env

namespace Cean

-- 通过递归定义算术表达式的求值
def evalA (env : Env) : AExpr → Except String Int
  | .const n =>      -- 表达式是常数
      .ok n          -- 携带正常值

  | .var name =>
      match Env.get env name with
      | some value =>
          .ok value
      | none =>     -- 一旦出现未定义的变量，返回错误
          .error s!"undefined variable: {name}"

  | .add lhs rhs => do   -- 表达式递归 左 + 右
      let l ← evalA env lhs
      let r ← evalA env rhs
      pure (l + r)

  | .sub lhs rhs => do
      let l ← evalA env lhs
      let r ← evalA env rhs
      pure (l - r)

  | .mul lhs rhs => do
      let l ← evalA env lhs
      let r ← evalA env rhs
      pure (l * r)


def evalB (env : Env) : BExpr → Except String Bool
  | .eq lhs rhs => do
      let l ← evalA env lhs
      let r ← evalA env rhs
      pure (l == r)

  | .less lhs rhs => do
      let l ← evalA env lhs
      let r ← evalA env rhs
      pure (decide (l < r))

  | .and lhs rhs => do    -- 短路
      let l ← evalB env lhs
      if l then
        evalB env rhs
      else
        pure false

  | .not e => do
      let value ← evalB env e
      pure (!value)

end Cean


/- ============================================================
   自测
   ============================================================ -/

open Cean

private def testEnv : Env :=
  Env.set (Env.set Env.empty "x" 3) "y" 4

-- 算术表达式
#guard (evalA testEnv (.add (.const 1) (.mul (.const 2) (.const 3)))).toOption == some 7
#guard (evalA testEnv (.sub (.const 1) (.const 4))).toOption == some (-3)
#guard (evalA testEnv (.var "x")).toOption == some 3

-- 未定义变量报错
#guard (evalA testEnv (.var "z")).isOk == false

-- 比较
#guard (evalB testEnv (.less (.var "x") (.var "y"))).toOption == some true
#guard (evalB testEnv (.less (.var "y") (.var "x"))).toOption == some false
#guard (evalB testEnv (.eq (.var "x") (.var "x"))).toOption == some true
#guard (evalB testEnv (.eq (.var "x") (.var "y"))).toOption == some false

-- 取反
#guard (evalB testEnv (.not (.less (.const 1) (.const 0)))).toOption == some true

-- && 短路：右侧引用了未定义变量，但左侧已经是 false，所以右侧不应被求值
#guard
  (evalB testEnv
    (.and (.less (.const 1) (.const 0))
          (.eq (AExpr.var "undefined") (.const 0)))).toOption
    == some false

-- 对照：左侧为 true 时右侧会被求值，于是报未定义变量
#guard
  (evalB testEnv
    (.and (.less (.const 0) (.const 1))
          (.eq (AExpr.var "undefined") (.const 0)))).isOk
    == false
