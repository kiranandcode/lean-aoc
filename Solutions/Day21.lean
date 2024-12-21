import Batteries
import Utils


def input := AOC.getInput 21
def testInput := "029A
980A
179A
456A
379A"


abbrev Grid := Array (Array Char)
abbrev HMap A B [Hashable A] [BEq A] := Std.HashMap A B
abbrev HSet A [Hashable A] [BEq A] := Std.HashSet A

structure NumPad where
  grid : Grid
  pos : Coord
deriving Repr, Inhabited, BEq

def String.toNumPad : String -> NumPad :=
  fun s =>
    let grid := s.toGrid 
    {grid := grid, pos := grid.find2D? (· == 'A') |>.get! }

def Direction.toChar : Direction -> Char
| .Up => '^'
| .Down => 'v'
| .Left => '<'
| .Right => '>'

def pathTo (g: Grid) (startPos: Coord) (endCoord: Coord) : List Direction := Id.run $ do
   let mut steps := #[]
   let mut currentCoord := startPos
   let mut prevDir : Option Direction := Option.none
   while currentCoord != endCoord do
      let currentDist := currentCoord.manhattenDistance endCoord
      let nextDirCoords :=
         Directions
         |>.filterMap (fun d => d.move currentCoord |>.map (d, ·))
         |>.filter (fun (d,c) => g.inBounds c && g.get2D! c != ' ')
         |>.filter (fun (d,c) => c.manhattenDistance endCoord < currentDist)
         |>.toHashMap
      let (dir, newPos) :=
         if prevDir.isSome && nextDirCoords.contains prevDir.get!
         then (prevDir.get!, nextDirCoords[prevDir.get!]!)
         else let key := nextDirCoords.first!; (key, nextDirCoords[key]!)
      currentCoord := newPos
      steps := steps.push dir
      prevDir := some dir
   return steps.toList

def NumPad.moveTo (op: Char) (numpad: NumPad) : NumPad × List Direction := Id.run $ do
    let newPos := numpad.grid.find2D? (· == op) |>.get!
    let moves := pathTo numpad.grid numpad.pos newPos
    ({numpad with pos := newPos}, moves)

def numpad1 := "789
456
123
 0A".toNumPad

def numpad2 := " ^A
<v>".toNumPad

def buildInputSeq (numpad: NumPad) (input: List Char) :=
  input.foldl (fun (numpad, dirs) v => 
      let (numpad, dir) := numpad.moveTo v
      (numpad, dirs.cons (dir.map (fun (d: Direction) => d.toChar) ++ ['A']))
  ) (numpad,[])
  |> (List.flatten ∘ List.reverse ∘ Prod.snd)

def complexity (s: String) :=
   let lengthOfShortest :=
       s.toList
       |> buildInputSeq numpad1
       |> buildInputSeq numpad2
       |> buildInputSeq numpad2
       |> List.length
   let digit := s.toList.filter (·.isDigit) |> String.mk |>.toNat!
   (digit, lengthOfShortest)

#eval "379A".toList
      |> buildInputSeq numpad1

-- 68 * 29, 60 * 980, 68 * 179, 64 * 456, and 64 * 379
#eval testInput.splitLines
      |>.map complexity


