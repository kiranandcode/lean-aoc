import Batteries
import Utils
open Std

def input := AOC.getInput 2
def testInput := "11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124"


def parseInput (input: String) :=
  input.splitOn ","
  |>.map String.trim
  |>.flatMap
     (·.splitOn "-"
      |>.map (·.toNat!)
      |>.take2.toList)

def processInput isInvalidId (input: String) :=
  parseInput input
  |>.map (fun (st, ed) => (Range.mk st (ed + 1) (step := 1) (by grind)))
  |>.flatMap (·.toList)
  |>.filter isInvalidId
  |>.sum

def isInvalidIdP1 (id: Nat) : Bool :=
  let mask := (10 ^ (id.noDigits / 2))
  id / mask == id % mask

def isInvalidIdP2 (id: Nat)  :=
   let id := s!"{id}"
   id.length.factors
   |>.any (fun factor =>
      id.partitionIntoN factor
      |> Std.HashSet.ofArray
      |> (·.size == 1)
   )

#example processInput isInvalidIdP1 testInput evaluates to 1227775554
#example processInput isInvalidIdP2 testInput evaluates to 4174379265

