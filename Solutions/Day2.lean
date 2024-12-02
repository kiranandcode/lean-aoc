import Batteries
import Utils

def input : IO String := AOC.getInput 2

def parseInput (input: String) :=
   input.splitLines
   |>.map String.words
   |>.map (List.map String.toInt!)

def differences (ls: List Int) := ls.zipWith Int.sub ls.tail

def exampleInput : String :=
"7 6 4 2 1
1 2 7 8 9
9 7 6 2 1
1 3 2 4 5
8 6 4 4 1
1 3 6 7 9"

def isSafeSequence (s: List Int) :=
   let diffs := differences s
   (diffs.all Int.isPos || diffs.all Int.isNeg) && diffs.all (Int.inRange ·.natAbs 1 3)
def process (s: String) :=
   parseInput s
   |>.countP isSafeSequence

#example process exampleInput evaluates to 2
#example process <$> input evaluates to 463

def isSafeSequence' (line: List Int) : Bool := 
   let rec foldSequence (cmp: Int -> Int -> Bool) (ls: List Int) : List (Bool × Int) := 
         match ls with
         | [] | [_] =>  []
         | [v1, v0] =>
            (if cmp v0 v1 then [(false, v1)] else []) ++ [(true, v0), (true, v1)]
         | vi :: rest => 
           foldSequence cmp rest
           |>.flatMap fun (skipUsed, lastVl) =>
                 (if cmp lastVl vi then [(skipUsed,vi)] else []) ++
                 (if skipUsed then [] else [(true, lastVl)])
   let increasing v0 v1 := v0 < v1 && (v1 - v0) <= 3
   let decreasing v0 v1 := v0 > v1 && (v0 - v1) <= 3
   let increasingLs := (foldSequence increasing line.reverse)
   let decreasingLs := (foldSequence decreasing line.reverse)
   !increasingLs.isEmpty || !decreasingLs.isEmpty


#example isSafeSequence' [0, 3, 6] evaluates to true
#example isSafeSequence' [0, 3, 8, 6] evaluates to true
#example isSafeSequence' [0, 8, 3, 6] evaluates to true
#example isSafeSequence' [1, 0, 3, 6] evaluates to true

#example isSafeSequence' [0, 10, 1, 2, 5] evaluates to true

def process' (input: String) : Nat :=
   parseInput input |>.countP isSafeSequence'

#example process' exampleInput evaluates to 4
#example process' <$> input evaluates to 514
