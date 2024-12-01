import Batteries
import Utils

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
     i
     |>.splitOn "\n"
     |>.map (·.trim)
     |>.filter (not ∘ String.isEmpty)
     |>.map parseLine
     |>.unzip
   let left := left.mergeSort (fun l r => l <= r)
   let right := right.mergeSort (fun l r => l <= r)
   left.zipWith (fun l r => Int.natAbs (l - r)) right
   |>.sum

#example process exampleInput
evaluates to 11

#example process <$> input
  evaluates to 1938424

def process' (i: String) :=
   let (left, right) :=
     i
     |>.splitOn "\n"
     |>.map (·.trim)
     |>.filter (not ∘ String.isEmpty)
     |>.map parseLine
     |>.unzip
  let rMap := right.foldr
          (fun v (m: Std.HashMap Int Nat) =>
             m.alter v (fun n => some <| n.getD 0 + 1))
          .empty
  left
  |>.map (fun v => rMap.getD v 0 * v)
  |>.sum
   

#example process' exampleInput
   evaluates to (31: Int)

#example process' <$> input
  evaluates to (22014209: Int)

