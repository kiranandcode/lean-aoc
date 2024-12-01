import Batteries
import Utils

def parseArgs (args: List String) : IO (Int × Int) := do
  let currentYear <- AOC.Date.getYear
  match args with
  | [day, year] => 
     let some day := day.toInt?.filter fun v => 1 <= v && v <= 25
       | throw <| IO.userError s!"argument {day} was not a day between 1 and 25"
     let some year := year.toInt?.filter fun v => 2015 <= v && v <= currentYear
       | throw <| IO.userError s!"argument {year} was not a valid year between 2015 && 2024"
     return (year,day)
  | [day] =>
     let some day := day.toInt?.filter fun v => 1 <= v && v <= 25
       | throw <| IO.userError s!"argument {day} was not a day between 1 and 25"
     let year <- AOC.Date.getYear
     return (year, day)
  | _ =>
     let year <- AOC.Date.getYear
     let day <- AOC.Date.getDay
     return (year, day)

def main (args : List String) : IO Unit := do
  let ⟨year, day⟩ <- parseArgs args
  let _ <- AOC.getInput day (year:=.some year)
  return 
