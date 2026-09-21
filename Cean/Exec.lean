import Cean.Stmt
import Cean.Eval
import Cean.Env

namespace Cean

def step
    (env : Env)
    (todo : List Stmt) :
    Except String (Env × List Stmt) := do

  match todo with

  | [] =>
      pure (env, [])

  | stmt :: rest =>
      match stmt with

      | .skip =>
          pure (env, rest)

      | .decl name init =>
          let value ← evalExpr env init
          let env' ← Env.declare env name value
          pure (env', rest)

      | .assign name rhs =>
          let value ← evalExpr env rhs
          let env' ← Env.assign env name value
          pure (env', rest)

      | .block stmts =>
          pure (
            env,
            stmts ++ rest
          )

      | .ifThenElse cond thenBranch elseBranch =>
          let value ← evalExpr env cond

          if isTruthy value then
            pure (
              env,
              thenBranch :: rest
            )
          else
            pure (
              env,
              elseBranch :: rest
            )

      | .while cond body =>
          let value ← evalExpr env cond

          if isTruthy value then
            pure (
              env,
              body :: (.while cond body) :: rest
            )
          else
            pure (
              env,
              rest
            )


def run :
    Nat →
    Env →
    List Stmt →
    Except String Env

  | 0, env, [] =>
      .ok env

  | 0, _, _ :: _ =>
      .error "execution fuel exhausted"

  | .succ _, env, [] =>
      .ok env

  | .succ fuel, env, todo => do
      let (env', todo') ← step env todo
      run fuel env' todo'


def exec
    (fuel : Nat)
    (env : Env)
    (stmt : Stmt) :
    Except String Env :=
  run fuel env [stmt]

end Cean

-- Test

open Cean

/- int x = 1; while (x < 5) { x = x + 1; } -/
private def loopProgram : Stmt :=
  .block [
    .decl
      "x"
      (.intLit 1),

    .while
      (.binary .lt
        (.var "x")
        (.intLit 5))

      (.block [
        .assign
          "x"
          (.binary .add
            (.var "x")
            (.intLit 1))
      ])
  ]

-- 循环跑完后 x = 5
#guard
  ((exec
      1000
      Env.empty
      loopProgram).map
        (Env.get · "x")).toOption
  ==
  some (some 5)

-- fuel 不够时停下来并报错
#guard (exec 1 Env.empty loopProgram).isOk == false


/- block execution order -/
private def blockProgram : Stmt :=
  .block [
    .decl "x"
      (.intLit 1),

    .assign "x"
      (.binary .add
        (.var "x")
        (.intLit 2)),

    .assign "x"
      (.binary .mul
        (.var "x")
        (.intLit 10))
  ]

-- (1 + 2) * 10 = 30，验证 block 内顺序执行
#guard
  ((exec
      100
      Env.empty
      blockProgram).map
        (Env.get · "x")).toOption
  ==
  some (some 30)


/- if (0 < n) { int r = 1; } else { int r = 2; } -/
private def branchProgram (n : Int) : Stmt :=
  .ifThenElse
    (.binary .lt (.intLit 0) (.intLit n))
    (.block [.decl "r" (.intLit 1)])
    (.block [.decl "r" (.intLit 2)])

-- 条件为真走 then 分支
#guard
  ((exec 100 Env.empty (branchProgram 1)).map (Env.get · "r")).toOption
    == some (some 1)

-- 条件为假走 else 分支
#guard
  ((exec 100 Env.empty (branchProgram (-1))).map (Env.get · "r")).toOption
    == some (some 2)


-- 未声明就赋值，运行期报错
#guard (exec 100 Env.empty (.assign "nope" (.intLit 1))).isOk == false

-- 空语句什么都不做
#guard ((exec 100 Env.empty .skip).map (Env.get · "x")).toOption == some none
