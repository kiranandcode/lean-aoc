import Batteries
import Utils.String

namespace AOC.Date

def IO.Process.runCmdOutputInt (cmd: String) (args: Array String) : IO Int := do
  let res <- IO.Process.runCmdWithInput cmd args
  let some res := res.trim.toInt?
    | throw (IO.userError s!"expecting {res} stdout from \"{cmd} {String.concat args.toList " "}\" to be an integer")
  return res

def getYear : IO Int := do
  IO.Process.runCmdOutputInt "date" #["+%Y"]

def getDay : IO Int := do
  IO.Process.runCmdOutputInt "date" #["+%-d"]

def getTime : IO Int := do
   IO.Process.runCmdOutputInt "date" #["+%s"]

end AOC.Date
