namespace Cean.LLVM

-- 产物目录
def outputDir : System.FilePath :=
  "output"

def smokeIRPath : System.FilePath :=
  outputDir / "llvm-smoke.ll"

def smokeExePath : System.FilePath :=
  if System.Platform.isWindows then
    outputDir / "llvm-smoke.exe"
  else
    outputDir / "llvm-smoke"


-- Lean → .ll → clang → executable

def smokeIR : String :=
"define i32 @main()
{
  entry:
    ret i32 42
}
"

def ensureOutputDir : IO Unit := do
  IO.FS.createDirAll outputDir


def emitSmokeIR : IO System.FilePath := do
  ensureOutputDir

  IO.FS.writeFile
    smokeIRPath
    smokeIR

  pure smokeIRPath


def compileIR
    (irPath : System.FilePath)
    (exePath : System.FilePath) :
    IO (Except String Unit) := do

  try
    let result ← IO.Process.output {
      cmd := "clang"
      args := #[
        irPath.toString,
        "-o",
        exePath.toString
      ]
    }

    if result.exitCode == 0 then
      pure (.ok ())
    else
      pure (.error s!"clang failed:\n{result.stderr}")

  catch e =>
    pure (.error s!"failed to execute clang: {e}")


def runSmokeTest :
    IO (Except String (System.FilePath × System.FilePath)) := do

  let irPath ← emitSmokeIR

  match ← compileIR irPath smokeExePath with
  | .error msg =>
      pure (.error msg)

  | .ok () =>
      pure (.ok (irPath, smokeExePath))


end Cean.LLVM
