import Batteries
  
def cookie_path := "./cookie"
def cookie : IO String := IO.FS.readFile cookie_path
  
namespace String
def concat (s : List String) (sepBy := "") : String :=
   s.foldl (fun acc v => acc ++ sepBy ++ v) ""
end String

def getYear : IO Int := do
  let res <- IO.Process.runCmdWithInput "date" #["+%Y"]
  let some res := res.trim.toInt?
    | throw (IO.userError s!"expecting {res} stdout from \"date +%Y\" to be an integer")
  return res

def getDay : IO Int := do
  let res <- IO.Process.runCmdWithInput "date" #["+%-d"]
  let some res := res.trim.toInt?
    | throw (IO.userError s!"expecting {res} stdout from \"date +%-d\" to be an integer")
  return res


def parseArgs (args: List String) : IO (Int × Int) := do
  match args with
  | [day, year] => 
     let some day := day.toInt?.filter fun v => 1 <= v && v <= 25
       | throw <| IO.userError s!"argument {day} was not a day between 1 and 25"
     let some year := year.toInt?.filter fun v => 2015 <= v && v <= 2024
       | throw <| IO.userError s!"argument {year} was not a valid year between 2015 && 2024"
     return (year,day)
  | [day] =>
     let some day := day.toInt?.filter fun v => 1 <= v && v <= 25
       | throw <| IO.userError s!"argument {day} was not a day between 1 and 25"
     let year <- getYear
     return (year, day)
  | _ =>
     let year <- getYear
     let day <- getDay
     return (year, day)



def main (args : List String) : IO Unit := do
  let ⟨year, day⟩ <- parseArgs args
  let session_cookie <- cookie
  let res <-
     IO.Process.runCmdWithInput "echo" 
     #[ s!"https://adventofcode.com/{year}/day/{day}/input",
     "-k",
     "-H", "User-Agent: (https://github.com/kiranandcode; mail@kirancodes.me)",
     "-H", s!"Cookie: session={session_cookie}"
     ]
  IO.FS.createDir
  println! "{res}"

