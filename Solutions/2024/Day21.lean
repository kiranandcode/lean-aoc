import Batteries
import Utils


def input := AOC.getInput 21 (year := .some 2024)
def testInput := "029A
980A
179A
456A
379A"

#eval "029A"

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

def allInBounds? (g: Grid) startPos (dirs: List Direction) :=
    dirs.foldl (init:=(startPos, true)) (fun (pos, hitEmpty) dir =>
     let newPos := dir.move pos |>.get!
     (newPos, hitEmpty && (g.get2D! newPos != ' '))
    )
    |>.snd

def pathTo (g: Grid) (startPos: Coord) (endCoord: Coord) : List (List Direction) :=
  let dx :=
    if startPos.x < endCoord.x
    then List.replicate (endCoord.x - startPos.x) Direction.Down
    else if startPos.x > endCoord.x
    then  List.replicate (startPos.x - endCoord.x) Direction.Up
    else []
  let dy := 
    if startPos.y < endCoord.y
    then List.replicate (endCoord.y - startPos.y) Direction.Right
    else if startPos.y > endCoord.y
    then  List.replicate (startPos.y - endCoord.y) Direction.Left
    else []
  (if dx.isEmpty
  then [dy]
  else if dy.isEmpty
  then [dx]
  else [dx ++ dy, dy ++ dx])
  |>.filter (allInBounds? g startPos)

def NumPad.setPos (op: Char) (numpad: NumPad) : NumPad := Id.run $ do
    let newPos := numpad.grid.find2D? (· == op) |>.get!
    {numpad with pos := newPos}

def NumPad.moveTo (op: Char) (numpad: NumPad) : NumPad × List (List Direction) := Id.run $ do
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
      (numpad, dirs.cons (dir.map (fun (dirs: List Direction) => dirs.map (·.toChar) ++ ['A'])))
  ) (numpad,[])
  |>.snd
  |>.reverse
  |>.combinations
  |>.map List.flatten

def complexity (s: String) :=
   let lengthOfShortest := s.toList
      |> buildInputSeq numpad1
      |>.flatMap (buildInputSeq numpad2)
      |>.flatMap (buildInputSeq numpad2)
      |>.map List.length
      |>.min?
      |>.get!
   let digit := s.toList.filter (·.isDigit) |> String.mk |>.toNat!
   digit * lengthOfShortest

def process (input: String) :=
   input.splitLines
   |>.map complexity
   |>.sum

-- 68 * 29, 60 * 980, 68 * 179, 64 * 456, and 64 * 379
#example process testInput evaluates to 126384
#example process <$> input evaluates to 179444

abbrev Cache := HMap (Nat × Char × Char) Nat

def expandM (depth: Nat) (prevChar: Char) (char: Char) : StateT Cache Id Nat := do
  match (<- get)[(depth,prevChar,char)]? with
  | .some v => return v
  | .none =>
    let ls := numpad2.setPos prevChar |>.moveTo char |>.snd
             |> List.map (List.map Direction.toChar)
             |> List.map (List.append · ['A'])
    let res <- do
        if depth = 0
        then return ls.map List.length |>.min?.get!
        else ls.mapM (List.foldMapM
                     (fun prevChar v =>
                       expandM (depth - 1) prevChar v
                       |> Functor.map (·,v)) 'A')
            |> Functor.map (Option.get! ∘ List.min? ∘ List.map (List.sum ∘ Prod.fst))
    set ((<-get).insert (depth,prevChar,char) res)
    return res

def expandSeq (depth: Nat) (s: List Char) : StateT Cache Id Nat := do
  s.foldMapM (fun prevChar char =>
          expandM depth prevChar char
          |> Functor.map (·,char) ) 'A'
  |> (Functor.map (List.sum ∘ Prod.fst))

def lengthOfExpanded (depth: Nat) (s: String) : StateT Cache Id Nat :=
  s.toList
  |> buildInputSeq numpad1
  |>.mapM (expandSeq depth)
  |> Functor.map (Option.get! ∘ List.min?)

def digitOfExpanded (s: String) :=
   s.toList.filter (·.isDigit) |> String.mk |>.toNat!

def complexity' (depth: Nat) (s: String) := do
   let len <- lengthOfExpanded depth s
   return (len * digitOfExpanded s)

def process' (depth: Nat) (input: String) : Nat :=
   flip StateT.run' Std.HashMap.empty $ input.splitLines
   |>.mapM (complexity' depth)
   |> Functor.map List.sum

#example process' 24 testInput evaluates to 154115708116294
#example process' 24 <$> input evaluates to 223285811665866
