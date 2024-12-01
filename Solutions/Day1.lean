import Batteries
import Utils
open Std

def input :=  AOC.getInput 1

def exampleInput :=
"3   4
4   3
2   5
1   3
3   9
3   3"

-- #eval input

def parseLine (line: String) :=
  let elts := line.splitOn " "
    |>.filter (not ∘ String.isEmpty)
    |>.map String.toInt!
  (elts[0]!, elts[1]!)

def process (i: String) :=
   let (left, right) :=
     i.splitLines
     |>.map parseLine
     |>.unzip
   let left := left.mergeSort
   let right := right.mergeSort
   left.zipWith (fun l r => l - r) right
   |>.map Int.natAbs
   |>.sum

#example process exampleInput
evaluates to 11

#example process <$> input
  evaluates to 1938424

def process' (i: String) :=
   let (left, right) :=
     i.splitLines
     |>.map parseLine
     |>.unzip
  let rMap :=
    flip right.foldr HashMap.empty
      (fun v m => m.update v (·.get? + 1))
  left
  |>.map (fun v => rMap.getD v 0 * v)
  |>.sum
   

#example process' exampleInput
   evaluates to (31: Int)

#example process' <$> input
  evaluates to (22014209: Int)
