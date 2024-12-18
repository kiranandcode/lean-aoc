import Batteries
import Utils

def input := AOC.getInput 18
def testInput := "5,4
4,2
4,5
3,0
2,1
6,3
2,4
1,5
0,6
3,3
2,6
5,1
1,2
5,5
2,5
6,5
1,4
0,4
6,4
1,1
6,1
1,0
0,5
1,6
2,0"

abbrev Grid := Array (Array Char)
abbrev HMap A B [Hashable A] [BEq A] := Std.HashMap A B
abbrev HSet A [Hashable A] [BEq A] := Std.HashSet A

def parseInput (input: String) : List (Nat × Nat) :=
 input.splitLines |>.flatMap (·.splitOn "," |>.map String.toNat!)
 |>.take2
 |>.toList

def neigbours (g: Grid) (c: Coord) : List Coord :=
   g.neigboursOf (c.swap)
   |>.filter (fun pos => g.get2D! pos != '#')
   |>.map (fun pos => pos.swap)

def djikstra (g: Grid) (start: Coord) (endPos: Coord) : Id Nat := do
   let mut scores : HMap Coord Nat := Std.HashMap.empty.insert start 0
   let mut queue := [start]
   while !queue.isEmpty do
       let elt := queue.head!
       queue := queue.tail!
       let eltScore := scores[elt]!
       for neigbour in neigbours g elt do

          let neigbourScore := eltScore + 1
          let neigbourOldScore := scores[neigbour]?
          if neigbourOldScore.isNone || neigbourOldScore.get! > neigbourScore then
              scores := scores.insert neigbour neigbourScore
              queue := queue.insertSorted (fun a b => scores[a]! <= scores[b]!) neigbour
          if neigbour == endPos then
            return scores[neigbour]!
    return 0

             
def solve (w h: Nat) (len: Nat) (input: String) : Nat := Id.run $ do
   let mut grid := Array.mkArray h (Array.mkArray w '.')
   for elt in parseInput input |>.take len do
      grid := grid.set2D! (elt.swap) '#'
   let res <- djikstra grid (0,0) (w - 1, h - 1)
   return res

#example solve 7 7 12 testInput evaluates to 22
#example solve 71 71 1024 <$> input evaluates to 432


   
def djikstraPath (g: Grid) (start: Coord) (endPos: Coord) : Id (HSet Coord) := do
   let mut prev : HMap Coord Coord := Std.HashMap.empty
   let mut scores : HMap Coord Nat := Std.HashMap.empty.insert start 0
   let mut queue := [start]
   while !queue.isEmpty do
       let elt := queue.head!
       queue := queue.tail!
       let eltScore := scores[elt]!
       for neigbour in neigbours g elt do

          let neigbourScore := eltScore + 1
          let neigbourOldScore := scores[neigbour]?
          if neigbourOldScore.isNone || neigbourOldScore.get! > neigbourScore then
              scores := scores.insert neigbour neigbourScore
              prev := prev.insert neigbour elt
              queue := queue.insertSorted (fun a b => scores[a]! <= scores[b]!) neigbour
          if neigbour == endPos then
            break
    let mut result : HSet Coord := .empty
    let mut prevPos := prev[endPos]?
    while prevPos.isSome do
        result := result.insert prevPos.get!
        prevPos := prev[prevPos.get!]?
    return result

def solve' (w h: Nat) (input: String) : Coord := Id.run $ do
   let mut grid := Array.mkArray h (Array.mkArray w '.')
   let mut currentPath := djikstraPath grid (0,0) (w - 1, h - 1)
   for elt in parseInput input do
      grid := grid.set2D! (elt.swap) '#'
      if currentPath.contains elt then
          currentPath := djikstraPath grid (0,0) (w-1, h-1)
      if currentPath.isEmpty then
          return elt
   return (0,0)

#example solve' 7 7 testInput evaluates to (6,1)
def main :IO Unit := do
   let res <- solve' 71 71 <$> input
   println!"{res}"
