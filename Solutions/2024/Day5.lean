import Batteries
import Utils

def input : IO String := AOC.getInput 5 (year := .some 2024)

def exampleInput : String := "47|53
97|13
97|61
97|47
75|29
61|13
75|53
29|13
97|29
53|29
61|53
97|53
61|29
47|13
75|47
97|75
47|61
75|61
47|29
75|13
53|13

75,47,61,53,29
97,61,53,29,13
75,29,13
75,97,47,61,53
61,13,29
97,13,75,29,47"

def parseOrder := open Std.Internal.Parsec in
   open Std.Internal.Parsec.String in do
   let n1 <- digits
   skipChar '|'
   let n2 <- digits
   return (n1, n2)


def parseInput (s: String) := Id.run $ do
  let [ins,val] := s.splitOn "\n\n"
     | return ([],[])
  let ins :=
    ins.splitLines
    |>.map (parseOrder.run)
    |>.filterMap (·.toOption)
  let val :=
     val.splitLines
     |>.map (·.splitOn "," |>.filterMap String.toNat?)
  (ins, val)

def isInOrder (insMap: Std.HashMap Nat (List Nat)) (ls: List Nat) : Bool := Id.run $ do
     -- convert ls into a map from n to indexes where it occurs
     let lsMap := ls.enum.map Prod.swap |>.toHashMapMulti
     -- loop through lsMap, and check that
     for (page, occursIn) in lsMap do
       let aftPages := insMap.getD page []
       for page in aftPages.flatMap (flip Option.getD [] ∘ lsMap.get?) do
           if occursIn.any (· > page) then
              return false
     return true

def process (s: String) :=
  let (ins,val) := parseInput s
  -- convert ins to map v -> vls that must occur after v
  let insMap := ins.toHashMapMulti
  val.filter (isInOrder insMap)
  |>.map List.midElement
  |>.sum

#example process exampleInput evaluates to 143
#example process <$> input evaluates to 5651

def printQueueOrder (insMap: Std.HashMap Nat (List Nat)) (a: Nat) (b: Nat) : Bool :=
     if insMap.get? a |>.map (List.contains · b) |>.getD false
     then true
     else if insMap.get? b |>.map (List.contains · a) |>.getD false
     then false
     else a < b
   
def makeCorrect
   (insMapOriginal: Std.HashMap Nat (List Nat))
   (ls: List Nat) : List Nat := 
  Id.run $ do
     let insMap :=
        insMapOriginal.filter (fun k _ => ls.contains k)
        |>.map (fun _ v => v.filter ls.contains)
        |>.transitiveClosure
     let lsMap := ls.enum.map Prod.swap |>.toHashMapMulti
     -- map for each page to the minimum index it should occur at
     let mut minInd : Std.HashMap Nat Nat := .empty
     for page in lsMap.keys do
       let occursIn := lsMap.get! page
       -- get the last occurance of node (factor in min ind)
       let lastOccur := occursIn.max?.get?.max (minInd.get? page |>.get?)

       -- get pages that must occur after
       let aftPages := insMap.getD page []
       for aftPage in aftPages do
           -- find locations where aft page occurs
           let aftPageIndexes := lsMap[aftPage]?.get?
           let minIndex := aftPageIndexes.min?.get?
           -- if it occurs before last occurance of char
           let minIndex := if aftPageIndexes.any (· < lastOccur) then
                             -- earliest it can occur is before 
                             lastOccur
                           else
                             minIndex
           -- update minInd for aftPage accordingly
           minInd := minInd.update aftPage (fun v => v.getD minIndex |>.max minIndex)
     let ls :=
          (<- ls.enum
          |>.foldMapM (fun (acc: Std.HashMap Nat (List Nat)) ((i,v): Nat × Nat) =>
             let heldElts := acc.get? i |>.get?
             match minInd.get? v with
             | none => return (([v] ++ heldElts).mergeSort (printQueueOrder insMap), acc)
             | some minInd => do
                 if i < minInd
                 then return (heldElts.mergeSort (printQueueOrder insMap),
                      acc.update minInd (fun ls => ls.getD [] |>.cons v))
                 else return (([v] ++ heldElts).mergeSort (printQueueOrder insMap), acc)
             ) (Std.HashMap.empty))
          |>.fst
          |>.flatten
     return ls

def process' (s: String) : Nat := 
  let (ins,val) := parseInput s
  -- convert ins to map v -> vls that must occur after v
  let insMap := ins.toHashMapMulti
  val.filter (not $ isInOrder insMap ·)
  |>.map (makeCorrect insMap)
  |>.map (·.midElement)
  |>.sum

#example process' exampleInput evaluates to 123

#example process' <$> input evaluates to 4743
