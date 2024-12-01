import Batteries
import Utils.Date

open Lean
open Batteries

namespace AOC.File

def COOKIE_PATH  : System.FilePath := "./cookie"
def INPUT_PATH   : System.FilePath := "./inputs"
def LAST_SUBMISSION_TIME_PATH : System.FilePath := INPUT_PATH / "last_submission_time"

def getCookie : IO String := do
  if ! (<- COOKIE_PATH.pathExists) then
     throw (IO.userError s!"cookie file at {COOKIE_PATH} does not exist. Please retrieve your cookie form www.adventofcode.com")
  IO.FS.readFile COOKIE_PATH

def ensureDir (d: System.FilePath) : IO Unit := do
  if <- System.FilePath.pathExists d
  then
     if !(<- System.FilePath.isDir d) then
        throw (IO.userError s!"Path {d}, exists, but is not a directory")
     return
  else
     IO.FS.createDir d

def ensureInputDir := ensureDir INPUT_PATH
def ensureInputYearDir (year: Int) := ensureDir (INPUT_PATH / s!"{year}")

def checkInputExists (day: Int) (year : Option Int := .none ) : IO Bool := do
  let year <- (if let some year := year then return year else AOC.Date.getYear)
  ensureInputDir;
  ensureInputYearDir year;
  (INPUT_PATH / s!"{year}" / s!"{day}").pathExists

def writeInput (day: Int) (year : Option Int := .none ) (res: String) : IO Unit := do
  let year <- (if let some year := year then return year else AOC.Date.getYear)
  ensureInputDir;
  ensureInputYearDir year;
  IO.FS.writeFile (INPUT_PATH / s!"{year}" / s!"{day}") res

def getInput (day: Int) (year : Option Int := .none ) : IO String := do
  let year <- (if let some year := year then return year else AOC.Date.getYear)
  ensureInputDir;
  ensureInputYearDir year;
  IO.FS.readFile (INPUT_PATH / s!"{year}" / s!"{day}")
  

def getLastSubmissionTime : IO (Option Int) := do
  ensureInputDir;
  if <- LAST_SUBMISSION_TIME_PATH.pathExists
  then
     let last_time <- IO.FS.readFile LAST_SUBMISSION_TIME_PATH
     return last_time.trim.toInt?
  else
     return .none

def recordSubmission : IO Unit := do
  ensureInputDir
  let currentTime <- AOC.Date.getTime
  IO.FS.writeFile LAST_SUBMISSION_TIME_PATH s!"{currentTime}"

end AOC.File
