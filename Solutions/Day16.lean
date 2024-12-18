import Batteries
import Utils

def input := AOC.getInput 16
def testInput := "###############
#.......#....E#
#.#.###.#.###.#
#.....#.#...#.#
#.###.#####.#.#
#.#.#.......#.#
#.#.#####.###.#
#...........#.#
###.#.#####.#.#
#...#.....#.#.#
#.#.#.###.#.#.#
#.....#...#.#.#
#.###.#.#.#.#.#
#S..#.....#...#
###############"

abbrev Grid := Array (Array Char)
abbrev HMap A B [BEq A] [Hashable A] := Std.HashMap A B
abbrev HSet A [BEq A] [Hashable A] := Std.HashSet A

def neighbours (g: Grid) (p: Coord × Direction) : List ((Coord × Direction) × Nat) :=
   (p.snd.move p.fst |>.map (fun pos => [((pos, p.snd), 1)])).get? ++
   [((p.fst, p.snd.turnRight), 1000),((p.fst, p.snd.turnLeft), 1000)]   
  |>.filter (fun (p,_) => g.inBounds p.fst && g.get2D! p.fst != '#')


def djikstra (g: Grid) (init: (Coord × Direction)) (endPos: Coord) : Nat := Id.run $ do
   let mut scores : HMap (Coord × Direction) Nat :=
       Std.HashMap.empty.insert init 0
   let mut queue : List (Coord × Direction) := [init]
   while !queue.isEmpty do
      let elt := queue.head!
      queue := queue.tail!
      let scoreElt := scores[elt]!
      for (neigbour, cost) in neighbours g elt do
          let neigbourScore := scoreElt + cost
          let oldNeigbourScore := scores[neigbour]?
          if oldNeigbourScore.isNone || oldNeigbourScore.get! > neigbourScore then
            scores := scores.insert neigbour neigbourScore
            queue := queue.insertSorted (fun l r => scores[l]! <= scores[r]!) neigbour

          if neigbour.fst == endPos then
             return scores[neigbour]!
   return 0

def computeBestPoints (_g: Grid) (prev : HMap (Coord × Direction) (HSet (Coord × Direction)))
  (init: (Coord × Direction)) (endPos: HSet (Coord × Direction)) : Nat := Id.run $ do
  let mut bestCoords: HSet Coord := .empty
  let mut seenDirections : HSet (Coord × Direction) := .empty
  let mut queue := endPos.toArray
  while !queue.isEmpty do
     let elt := queue.back!
     queue := queue.pop
     bestCoords := bestCoords.insert elt.fst
     seenDirections := seenDirections.insert elt
     if elt != init then
       for prevElt in prev[elt]?.get? do
         if !seenDirections.contains prevElt then
           queue := queue.push prevElt
  return bestCoords.size
  

def djikstraAll (g: Grid) (init: (Coord × Direction)) (endPos: Coord) : Nat := Id.run $ do
   let mut scores : HMap (Coord × Direction) Nat :=
       Std.HashMap.empty.insert init 0
   let mut prev : HMap (Coord × Direction) (HSet (Coord × Direction)) := .empty
   let mut queue : List (Coord × Direction) := [init]
   let mut bestScore : Option Nat := none
   let mut bestScores : HSet (Coord × Direction) := .empty
   while !queue.isEmpty do
      let elt := queue.head!
      queue := queue.tail!
      let scoreElt := scores[elt]!
      for (neigbour, cost) in neighbours g elt do
          let neigbourScore := scoreElt + cost
          let oldNeigbourScore := scores[neigbour]?
          if oldNeigbourScore.isNone || oldNeigbourScore.get! >= neigbourScore then
            prev := 
               if oldNeigbourScore.isNone || oldNeigbourScore.get! > neigbourScore
               then prev.insert neigbour {elt}
               else prev.update neigbour (·.get?.insert elt)
            scores := scores.insert neigbour neigbourScore
            -- if we reached endPos no need to keep searching
            if neigbour.fst != endPos then
               queue := queue.insertSorted (fun l r => scores[l]! <= scores[r]!) neigbour
            else if bestScore.isNone then
               bestScore := some neigbourScore
               bestScores := bestScores.insert neigbour
            else if bestScore.get! >= neigbourScore then
               bestScores := bestScores.insert neigbour
   computeBestPoints g prev init bestScores


def process (input: String) := 
  let g := input.toGrid
  let sPos := g.find2D? (· == 'S') |>.get!
  let ePos := g.find2D? (· == 'E') |>.get!
  let dir := Direction.Right
  djikstra g (sPos,dir) ePos

#example process testInput evaluates to 7036
-- #example process <$> input evaluates to 99460

def process' (input: String) := 
  let g := input.toGrid
  let sPos := g.find2D? (· == 'S') |>.get!
  let ePos := g.find2D? (· == 'E') |>.get!
  let dir := Direction.Right
  djikstraAll g (sPos,dir) ePos

#example process' testInput evaluates to 45
-- #example process' <$> input evaluates to 500

def main : IO Unit := do
  let v <- process' <$> input
  println! "{v}"
  

