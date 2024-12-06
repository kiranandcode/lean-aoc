import Batteries
import Utils

abbrev Grid := Array (Array Char)
abbrev Coord := (Nat × Nat)

def input := AOC.getInput 6
def testInput :=
"....#.....
.........#
..........
..#.......
.......#..
..........
.#..^.....
........#.
#.........
......#..."

inductive Direction where
| Up
| Down
| Left
| Right
deriving Inhabited, BEq, Hashable, Repr

instance : ToString Direction where
   toString
   | .Up => "Up"
   | .Left => "Left"
   | .Down => "Down"
   | .Right => "Right"

def Direction.turnRight : Direction -> Direction
| .Up => .Right
| .Down => .Left
| .Left => .Up
| .Right => .Down

def Direction.turnLeft : Direction -> Direction
| .Up => .Left
| .Down => .Right
| .Left => .Down
| .Right => .Up


def Direction.ofChar? : Char -> Option Direction
| '^' => .some .Up
| '>' => .some .Right
| '<' => .some .Left
| 'v' => .some .Down
| _ => .none

def Direction.move (coord: Coord) : Direction -> Option Coord
| .Up => if coord.fst > 0 then some (coord.fst-1,coord.snd) else none
| .Down => some (coord.fst+1,coord.snd)
| .Left => if coord.snd > 0 then some (coord.fst,coord.snd - 1) else none
| .Right => some (coord.fst,coord.snd + 1)

def isDirection (c: Char) := c == '^' || c == '>' || c == '<' || c == '>'

def findDirectionInd (grid: Grid) :=
  flip StateT.run 0 $ (do
     grid.findIdxM? (fun a => do
       let idx := a.findIdx? (Option.isSome ∘ Direction.ofChar?)
       if let some idx := idx then
          StateT.set idx
          return true
       else return false
     ) : StateT Nat Id _)
     |>.map Option.get?

def inBounds (grid: Grid) (coord: Coord) :=
      coord.fst < grid.size && coord.snd < grid[0]!.size

def hasObstacle (grid:  Grid) (coord: Coord) :=
      grid[coord.fst]![coord.snd]! == '#'

def nextStep  (grid: Grid) (direction: Direction) (coord: Coord) : Option (Direction × Coord) := do
    let mut direction := direction
    for _ in [0:4] do
      let nextcoord <- direction.move coord
      if !inBounds grid nextcoord then none
      if !hasObstacle grid nextcoord then
         return (direction, nextcoord)
      direction := direction.turnRight
    none

def process (input: String) := Id.run do
   let grid := input.toGrid
   let mut pos := findDirectionInd grid
   let mut direction := Direction.ofChar? grid[pos.fst]![pos.snd]! |>.get?
   let mut step := nextStep grid direction pos
   let mut positions : Std.HashSet (Nat × Nat) := Std.HashSet.ofList [pos]
   while step.isSome do
      let (nextdirection, nextpos) := step.get!
      direction := nextdirection
      pos := nextpos
      step := nextStep grid direction pos
      positions := positions.insert pos
   return positions.size

#example process testInput evaluates to 41
#example process <$> input evaluates to 4663

def neighbours (g: Grid) (c: Coord) : List Coord :=
   [(c.fst + 1, c.snd)] ++
   [(c.fst, c.snd + 1)] ++
   (if c.fst > 0 then [(c.fst - 1, c.snd)] else []) ++
   (if c.snd > 0 then [(c.fst, c.snd - 1)] else [])
   |>.filter (inBounds g)

def pathLoops (grid: Grid) (direction: Direction) (coord: Coord) : Bool := Id.run $ do
  let mut coord := coord
  let mut direction := direction
  let mut step := nextStep grid direction coord
  let mut positionDirs := Std.HashSet.ofList [(coord, direction)]

  while step.isSome do
      let (newDirection, newCoord) := step.get!

      coord := newCoord
      direction := newDirection
-- if we hit a coord that we saw earlier, then this is a loop
      if positionDirs.contains (coord, direction) then
         return true
      positionDirs := positionDirs.insert (coord, direction)
      step := nextStep grid direction coord
  return false
  

def process' (input: String) : Nat := Id.run $ do
   let grid := input.toGrid
   let startPos := findDirectionInd grid
   let mut pos := startPos
   let mut direction := Direction.ofChar? grid[pos.fst]![pos.snd]! |>.get?
   let mut step := nextStep grid direction pos
   let mut positionDirs : Std.HashMap Coord (Coord × Direction) :=
      Std.HashMap.ofList [(pos, pos, direction)]

-- do the previous loop, but this time keep track of the
-- configurations in which we see each coord
   while step.isSome do
      let (nextdirection, nextpos) := step.get!
      -- keep the first direction we were moving in when we hit a piece on 
      positionDirs := positionDirs.update nextpos (fun s => s.getD (pos, direction))
      direction := nextdirection
      pos := nextpos
      step := nextStep grid direction pos

   let mut noPositions := 0
   for (coord, originalPos, dir) in positionDirs.erase startPos do
        let grid' := grid.set! coord.fst (grid[coord.fst]!.set! coord.snd '#')
        if (pathLoops grid' dir originalPos) then
            noPositions := noPositions + 1
   return noPositions

#example process' testInput evaluates to 6
-- #example process' <$> input evaluates to 1530

def main: IO Unit := do
   let res := process' (<- input)
   println! "{res}"
