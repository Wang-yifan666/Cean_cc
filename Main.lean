import Cean

open Cean


private def printVariables
    (program : Stmt)
    (env : Env) :
    IO Unit := do

  for name in Stmt.declaredNames program do

    match Env.get env name with

    | some value =>
        IO.println
          s!"{name} = {value}"

    | none =>
        pure ()


def main
    (args : List String) :
    IO UInt32 := do

  match args with

  | ["--llvm-smoke"] =>

      IO.println "=== LLVM Smoke Test ==="

      match ← Cean.LLVM.runSmokeTest with
      | .error msg =>
          IO.eprintln msg
          pure 1

      | .ok (irPath, exePath) =>
          IO.println s!"LLVM IR:   {irPath}"
          IO.println s!"Executable: {exePath}"
          IO.println "LLVM toolchain smoke test succeeded."
          pure 0

  | [filename] =>

      try

        let source ←
          IO.FS.readFile filename

        match parseSource source with
        | .error msg =>

            IO.eprintln
              s!"parse error: {msg}"

            pure 1

        | .ok program =>

            IO.println "=== AST ==="

            IO.println
              (reprStr program)

            IO.println
              "\n=== Running ==="

            match
              exec
                100000
                Env.empty
                program
            with

            | .error msg =>
                IO.eprintln
                  s!"runtime error: {msg}"

                pure 1

            | .ok env =>

                IO.println
                  "program finished successfully"

                IO.println
                  "\n=== Variables ==="

                printVariables
                  program
                  env

                pure 0

      catch e =>

        IO.eprintln
          s!"I/O error: {e}"

        pure 1

  | _ =>
      IO.eprintln
        "usage: cean <source.c> | cean --llvm-smoke"

      pure 1
