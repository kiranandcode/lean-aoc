import Batteries
import Utils

def input := AOC.getInput 7

def testInput := "190: 10 19
3267: 81 40 27
83: 17 5
156: 15 6
7290: 6 8 6 15
161011: 16 10 13
192: 17 8 14
21037: 9 7 18 13
292: 11 6 16 20"

def computePossibleResults (ls : List Nat) : List Nat :=
   if let hd :: tl := ls then
     flip tl.foldl {hd} (fun acc vl => Id.run $ do
        let mut acc' := Std.HashSet.empty
        for v in acc do
          acc' := acc'.insert (v + vl)
          acc' := acc'.insert (v * vl)
        return acc'
     )
     |>.toList
   else []

def process (input: String) := 
   input.splitLines
   |>.map (·.splitOn ":")
   |>.filterMap (match · with
      | [l,r] => some (l.toNat!, r.trim.splitOn " " |>.map String.toNat!)
      | _ => none
   )
   |>.filter (fun (res, ls) => computePossibleResults ls |>.contains res)
   |>.map (·.fst)
   |>.sum

#example process testInput evaluates to 3749
#example process <$> input evaluates to 5837374519342

-- I am a dumb dumb, I thought the concat operations split the entire list lol I am dumb  
def computePossibleResultsWithConcat (ls: List Nat) : List Nat := Id.run $ do
   let ls := ls.toArray
   let lsLength := ls.size
   -- possibleResults : Nat -> Nat -> Std.HashSet Nat
   let mut possibleResults : Std.HashMap (Nat × Nat) (Std.HashSet Nat) := Std.HashMap.empty
   -- possibleResults i i = ls[i]!
   for i in [0:lsLength] do
      possibleResults := possibleResults.insert (i,i) {ls[i]!}
   -- possibleResults i j =
   --    [vl | vl <- [v + ls[j]!, v * ls[j]!], v <- possibleResults i (j - 1) ]
   for i in [0:lsLength] do
      for j in [i + 1:lsLength] do
         let mut res : Std.HashSet Nat := {}
         for v in possibleResults[(i,j-1)]! do
            res := res.insertMany [v + ls[j]!, v * ls[j]!]
         possibleResults := possibleResults.insert (i,j) res
   let ls :=
     List.range lsLength |>.allPartitions
     |>.foldl (fun (solutions: Std.HashSet Nat) partition => 
        let result :=
           partition.foldl (fun (acc: Std.HashSet Nat) part => Id.run $ do
             let mut res := Std.HashSet.empty
             for outcome in possibleResults[(part.min?.get?,part.max?.get?)]! do
                for v in acc do
                  res := res.insert (v.combine outcome)
             return res
           ) ({0} : Std.HashSet Nat)
        solutions.union result
     ) Std.HashSet.empty
   return ls.toList

#example computePossibleResultsWithConcat [15, 6] evaluates to [156,90,21]
#example computePossibleResultsWithConcat [6, 8, 6, 15]
  evaluates to
  [629, 64815, 303, 1260, 300, 6890, 810, 48615, 61415,
   5415, 6821, 99, 35, 4320, 2015, 8415, 4890, 663, 14615,
   4821, 1490, 28815, 1421, 68615, 69, 6210, 6720]

def computePossibleResults' (ls : List Nat) : List Nat :=
   if let hd :: tl := ls then
     flip tl.foldl {hd} (fun acc vl => Id.run $ do
        let mut acc' := Std.HashSet.empty
        for v in acc do
          acc' := acc'.insert (v + vl)
          acc' := acc'.insert (v * vl)
          acc' := acc'.insert (v.combine vl)
        return acc'
     )
     |>.toList
   else []

def process' (input: String) := 
   input.splitLines
   |>.map (·.splitOn ":")
   |>.filterMap (match · with
      | [l,r] => some (l.toNat!, r.trim.splitOn " " |>.map String.toNat!)
      | _ => none
   )
   |>.filter (fun (res, ls) => computePossibleResults' ls |>.contains res)
   |>.map (·.fst)
   |>.sum

#example process' testInput evaluates to 11387
def main : IO Unit := do
  let res := process' (<- input)
  println! "{res}"
-- #example process' <$> input evaluates to 0
