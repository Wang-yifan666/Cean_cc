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
        "usage: cean <source.c>"

      pure 1
