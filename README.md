# Cean

> A C/C++ compiler project implemented in Lean 4.

Cean is an experimental compiler written from scratch in **Lean 4**.

The goal is to gradually build a practical C/C++ compiler while exploring **formal verification** of compiler semantics, transformations, optimizations, and code generation.

The project currently implements a small subset of C and is still evolving rapidly.

## Current Pipeline

```text
C Source
   ↓
Lexer
   ↓
Parser
   ↓
AST
   ↓
Interpreter
```

An LLVM toolchain path is also being introduced:

```text
Cean
  ↓
LLVM IR
  ↓
clang / LLVM
  ↓
Native Executable
```

Generated compiler artifacts are placed in:

```text
output/
```

## Build and Run

Install [elan](https://github.com/leanprover/elan), then:

```bash
git clone https://github.com/Wang-yifan666/Cean_cc.git
cd Cean_cc

lake build
```

Run a C source file:

```bash
lake exe cean examples/demo.c
```

Test the LLVM toolchain:

```bash
lake exe cean --llvm-smoke
```

## Current Status

Cean currently supports a small C-like language including:

* integer variables
* unified expressions
* arithmetic, comparison, and logical expressions
* `if / else`
* `while`
* block statements
* comments
* AST interpretation
* basic constant folding
* LLVM toolchain smoke testing

The frontend is currently being restructured before adding significantly more C syntax.

## Roadmap

```text
Frontend foundations
        ↓
Semantic analysis
        ↓
HIR / MIR
        ↓
SSA / CFG
        ↓
Optimization
        ↓
LLVM IR
        ↓
Native executable
        ↓
Formal verification
        ↓
More complete C
        ↓
C++
```

Near-term work focuses on lexical scopes, source locations, diagnostics, functions, types, and the first real AST-to-LLVM compilation pipeline.

Cean is intended to become both a compiler engineering project and a platform for experimenting with **formally verified compiler construction**.
