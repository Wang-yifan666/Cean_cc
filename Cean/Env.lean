namespace Cean

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

end Cean

/- ============================================================
   自测
   ============================================================ -/

open Cean

-- 创建一个空环境，依次放入 x = 10、y = 20
private def env0 : Env := Env.empty
private def env1 : Env := Env.set env0 "x" 10
private def env2 : Env := Env.set env1 "y" 20

#guard Env.get env2 "x" == some 10
#guard Env.get env2 "y" == some 20
-- 没有放入过的名字读到 none
#guard Env.get env2 "z" == none

-- set 只影响被写入的那个名字
#guard Env.get (Env.set env2 "x" 99) "x" == some 99
#guard Env.get (Env.set env2 "x" 99) "y" == some 20

-- 重复声明同名变量要报错
#guard (Env.declare env2 "x" 1).isOk == false

-- 声明新变量成功，并且能读回写入的值
#guard ((Env.declare env2 "z" 42).map (Env.get · "z")).toOption == some (some 42)

-- 未声明的变量不能赋值
#guard (Env.assign env2 "z" 1).isOk == false

-- 已声明的变量可以赋值，且覆盖旧值
#guard ((Env.assign env2 "x" 7).map (Env.get · "x")).toOption == some (some 7)
