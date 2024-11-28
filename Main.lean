import AdventOfCode
import Batteries
  
def cookie_path := "./cookie"

def cookie : IO String := IO.FS.readFile cookie_path
  
namespace String

def concat (s : List String) (sepBy := "") : String :=
   s.foldl (fun acc v => acc ++ sepBy ++ v) ""

end String



def main : IO Unit := do
  let year := 2023
  let day := 1
  let session_cookie <- cookie
  println! "https://adventofcode.com/{year}/day/{day}/input"
  let args :=
  #[ s!"https://adventofcode.com/{year}/day/{day}/input",
     "-k",
     "-H", "User-Agent: (https://github.com/kiranandcode; mail@kirancodes.me)",
     "-H", s!"Cookie: session={session_cookie}"
     ]
  println! "{args}"
  let res <-
     IO.Process.runCmdWithInput "curl" 
     args
  println! "{res}"

  IO.println s!"Hello, {hello}!"
