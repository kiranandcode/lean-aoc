import Batteries
import Utils.File
import Utils.Date

namespace AOC.Network

def getInputFor (day: Int) (year: Int) : IO String := do
  if ! 1 <= day && day <= 12 then
    throw <| IO.userError s!"day {day} was not a valid date"
  if ! 2015 <= year && year <= (<- AOC.Date.getYear) then
    throw <| IO.userError s!"year {year} was not a valid year"
  if <- AOC.File.checkInputExists day (year:=.some year) then
     AOC.File.getInput day (year := .some year)
  else
    let session_cookie <- AOC.File.getCookie
    let res <-
     IO.Process.runCmdWithInput "curl" 
     #[ s!"https://adventofcode.com/{year}/day/{day}/input",
     "-sS",
     "-k",
     "-H", "User-Agent: (https://github.com/kiranandcode; kirang@comp.nus.edu.sg)",
     "-H", s!"Cookie: session={session_cookie}"
     ]
    AOC.File.writeInput day (year:=.some year) res
    return res

def getInput (day: Int) (year: Option Int := .none) : IO String := do
  let year <- (if let some year := year then return year else AOC.Date.getYear)
  getInputFor day year


end AOC.Network
