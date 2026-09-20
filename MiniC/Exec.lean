import MiniC.Stmt
import MiniC.Eval
import MiniC.Env

namespace MiniC

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
          let value ← evalA env init
          let env' ← Env.declare env name value
          pure (env', rest)

      | .assign name rhs =>
          let value ← evalA env rhs
          let env' ← Env.assign env name value
          pure (env', rest)

      | .seq first second =>
          pure (
            env,
            first :: second :: rest
          )

      | .ifThenElse cond thenBranch elseBranch =>
          let result ← evalB env cond

          if result then
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
          let result ← evalB env cond

          if result then
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

end MiniC
