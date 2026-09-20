import MiniC.AST
import MiniC.Env

namespace MiniC

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

end MiniC
