# cean

用 [Lean 4](https://leanprover.github.io/) 从零实现 C / C++ 编译器。

> **目标**：一个能完整编译 C 和 C++ 的编译器。
> **现状**：早期阶段。已经打通「源码 → 词法分析 → 语法分析 → AST → 解释执行」这条链路，
> 覆盖一个很小的 C 子集；类型检查、IR、优化和代码生成都还没有。

An in-progress C/C++ compiler written from scratch in Lean 4.

## 构建与运行

需要 [elan](https://github.com/leanprover/elan)；Lean 版本由 `lean-toolchain` 固定为 `leanprover/lean4:v4.34.0`。

```bash
lake build                            # 编译
lake exe cean examples/demo.c         # 运行
```

运行时会依次打印 AST、执行结果和所有变量的终值：

```
$ lake exe cean examples/demo.c
=== AST ===
Cean.Stmt.seq
  ...
=== Running ===
program finished successfully

=== Variables ===
x = 999
y = 24
i = 10
```

## 目前支持的语言子集

整程序只支持一个入口：

```c
int main() {
  /* ... */
}
```

| 类别 | 支持 |
| --- | --- |
| 语句 | `int x = 表达式;`、`x = 表达式;`、`if (布尔) { } else { }`、`while (布尔) { }`、`{ }`、`;` |
| 算术 | `+` `-` `*`、括号、十进制整数字面量 |
| 布尔 | `<`、`==`、`&&`、`!` |
| 注释 | `//` 行注释、`/* */` 块注释 |
| 标识符 | 字母或 `_` 开头，后跟字母 / 数字 / `_` |

运算符优先级（从紧到松）：`*` → `+ -` → `< ==` → `!` → `&&`。

几处和 C 不一样的地方，读代码时需要注意：

- `!` 作用在**整条比较式**上：`!x < 10` 等价于 `!(x < 10)`，而不是 `(!x) < 10`。
- 比较运算符 `<` / `==` 必须出现，且只能出现一次。所以 `while (x) { }` 这种「布尔变量直接作条件」的写法不合法。
- 括号只能包住**算术**表达式，不能包住布尔表达式：`!(x == 0)` 会解析失败。
- `if` / `while` 的循环体必须是花括号块，不支持单语句体。

## 项目结构

| 文件 | 职责 |
| --- | --- |
| `Cean/AST.lean` | 抽象语法树：算术表达式 `AExpr`、布尔表达式 `BExpr` |
| `Cean/Stmt.lean` | 语句 `Stmt`，以及 `seqMany`、`declaredNames` 两个工具函数 |
| `Cean/Lexer.lean` | 词法分析：`String → List Token` |
| `Cean/Parser.lean` | 递归下降语法分析：`String → Stmt` |
| `Cean/Env.lean` | 运行期环境，`String → Option Int` 的函数式映射 |
| `Cean/Eval.lean` | 表达式求值（`&&` 短路） |
| `Cean/Exec.lean` | 语句的小步执行机，带 fuel 防止死循环 |
| `Cean/Optimize.lean` | 算术表达式的常量折叠（**尚未接入主流程**） |
| `Cean.lean` | 汇总导出 |
| `Main.lean` | 命令行入口 |

## 测试

各模块末尾用 `#guard` 写了断言，它们在 **`lake build` 期间执行**，断言失败会直接让构建失败：

```bash
lake build    # 断言不成立 => 构建失败
```

CI（`.github/workflows/lean_action_ci.yml`）使用官方的
[`leanprover/lean-action`](https://github.com/leanprover/lean-action)，
默认行为就是跑 `lake build`，因此这些断言在 CI 上是真正生效的。
目前还没有配置 Lake 的 `test_driver`，所以 `lake test` 是空的。

## 路线图

- [x] 词法分析、语法分析
- [x] 树遍历解释器（带小步执行机）
- [ ] 补全 C 的表达式与语句：一元运算、`/` `%`、`!=` `<=` `>=` `||`、`for`、`do-while`、`break` / `continue`
- [ ] 真正的块作用域（当前环境是扁平的）
- [ ] 接入常量折叠，并扩展到语句级优化
- [ ] 类型系统与语义检查
- [ ] 中间表示（三地址码 / SSA）
- [ ] 优化：死代码删除、常量传播、寄存器分配
- [ ] 目标代码生成（x86-64，或先做一个 C 后端）
- [ ] 多函数、函数调用、`return`、标准库
- [ ] C++ 前端

## 已知限制

这些都是当前实现的真实边界，不是笔误：

- **没有块作用域**：环境是扁平的 `String → Option Int`，内层块里声明的变量会泄漏到外层。
- **循环体内不能声明变量**：因为上一条，`while (...) { int j = 5; ... }` 会在第二轮迭代报
  `variable already defined: j`。
- 变量必须先声明后使用，且同一条路径上不能重复声明同名变量。
- 声明必须带初始化：`int x;` 不支持。
- 没有一元负号，所以**无法书写负数常量**。
- 除法 `/` 根本没有进词法器（`/` 只被当作注释开头）。
- 报错信息里没有行列号，只回显 token。
- 只有一个 `main` 函数，没有 `return`、没有函数调用、没有 `printf`。
- `Cean/Optimize.lean` 的常量折叠目前没有被任何地方调用。

## 许可

尚未添加 LICENSE 文件。
