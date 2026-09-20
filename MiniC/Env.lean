namespace MiniC

-- 实现变量名（string）到值（int）的映射
abbrev Env := String → Option Int

-- 传入任何变量名，返回默认值0
def Env.empty : Env :=
  fun _ => none

-- 取出 Env 的 Int 值
def Env.get (env : Env) (name : String) : Option Int :=
  env name

-- 设置 Env 的 Int 值，传入旧环境、变量名和新值，返回新的环境
-- 更新环境
def Env.set
    (env : Env)
    (name : String)
    (value : Int) : Env :=
  fun query =>
    if query = name then
      some value
    else
      env query

-- 声明变量
def Env.declare
    (env : Env)
    (name : String)
    (value : Int) : Except String Env :=
  match env name with
  | some _ =>
      .error s!"variable already defined: {name}"
  | none =>
      .ok (Env.set env name value)

-- 给变量赋值
def Env.assign
    (env : Env)
    (name : String)
    (value : Int) : Except String Env :=
  match env name with
  | some _ =>
      .ok (Env.set env name value)
  | none =>
      .error s!"undefined variable: {name}"

end MiniC

-- Test
open MiniC

-- 创建一个空环境
def env0 := Env.empty
def env1 := Env.set env0 "x" 10
def env2 := Env.set env1 "y" 20

#eval Env.get env2 "x"
#eval Env.get env2 "y"
#eval Env.get env2 "z"
