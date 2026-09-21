# Cean

> 一个用 Lean 4 实现的 C/C++ 编译器项目。

Cean 是一个用 **Lean 4** 从零开始编写的实验性编译器。

目标是逐步构建一个实用的 C/C++ 编译器，同时探索编译器语义、变换、优化和代码生成的**形式化验证**。

该项目目前实现了 C 的一个小子集，并且仍在快速演进。

## 当前流水线

```text
C 源代码
   ↓
词法分析器
   ↓
语法分析器
   ↓
AST
   ↓
解释器
```

LLVM 工具链路径也正在引入：

```text
Cean
  ↓
LLVM IR
  ↓
clang / LLVM
  ↓
原生可执行文件
```

生成的编译器产物放置在：

```text
output/
```

## 构建与运行

安装 [elan](https://github.com/leanprover/elan)，然后：

```bash
git clone https://github.com/Wang-yifan666/Cean_cc.git
cd Cean_cc

lake build
```

运行一个 C 源文件：

```bash
lake exe cean examples/demo.c
```

测试 LLVM 工具链：

```bash
lake exe cean --llvm-smoke
```

## 当前状态

Cean 目前支持一个小型类 C 语言，包括：

* 整数变量
* 统一表达式
* 算术、比较和逻辑表达式
* `if / else`
* `while`
* 块语句
* 注释
* AST 解释
* 基本常量折叠
* LLVM 工具链冒烟测试

在添加显著更多 C 语法之前，前端目前正在重构。

## 路线图

```text
前端基础
        ↓
语义分析
        ↓
HIR / MIR
        ↓
SSA / CFG
        ↓
优化
        ↓
LLVM IR
        ↓
原生可执行文件
        ↓
形式化验证
        ↓
更完整的 C
        ↓
C++
```

近期工作聚焦于词法作用域、源位置、诊断、函数、类型，以及首个真正的 AST 到 LLVM 编译流水线。

Cean 旨在同时成为一个编译器工程项目，以及一个用于实验**形式化验证的编译器构造**的平台。