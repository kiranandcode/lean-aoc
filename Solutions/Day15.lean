import Batteries
import Utils


def input := AOC.getInput 15


def testInput := "##########
#..O..O.O#
#......O.#
#.OO..O.O#
#..O@..O.#
#O#..O...#
#O..O..O.#
#.OO.O.OO#
#....O...#
##########

<vv>^<v^>v>^vv^v>v<>v^v<v<^vv<<<^><<><>>v<vvv<>^v^>^<<<><<v<<<v^vv^v>^
vvv<<^>^v^^><<>>><>^<<><^vv^^<>vvv<>><^^v>^>vv<>v<<<<v<^v>^<^^>>>^<v<v
><>vv>v^v^<>><>>>><^^>vv>v<^^^>>v^v^<^^>v^^>v^<^v>v<>>v^v^<v>v^^<^^vv<
<<v<^>>^^^^>>>v^<>vvv^><v<<<>^^^vv^<vvv>^>v<^^^^v<>^>vvvv><>>v^<<^^^^^
^><^><>>><>^^<<^^v>>><^<v>^<vv>>v>>>^v><>^v><<<<v>>v<v<v>vvv>^<><<>^><
^>><>^v<><^vvv<^^<><v<<<<<><^v<<<><<<^^<v<^^^><^>>^<v^><<<^>>^v<v^v<v^
>^>>^v>vv>^<<^v<>><<><<v<<v><>v<^vv<<<>^^v^>^^>>><<^v>>v^v><^^>>^<>vv^
<><^^>^^^<><vvvvv^v<v<<>^v<v>v<<^><<><<><<<^^<<<^<<>><<><^^^>^^<>^>v<>
^^>vv<^v^v<vv>^<><v<^v>^^^>>>^^vvv^>vvv<>>>^<^>>>>>^<<^v>^vvv<>^<><<v>
v^^>>><<^^<>>^v^<v^vv<>v^<<>^<^v^v><^<<<><<^<v><v<>vv>>v><v^<vv<>v^<<^"

abbrev Grid := Array (Array Char)
instance : ToString Grid where
  toString g :=
     g.toList.map (fun r => s!"{r.toList.asString}\n")
     |> String.concat

partial def Grid.move (g: Grid) (p: Coord) (d: Direction) : Option Grid := do
  let p' <- d.move p
  let v := g.get2D! p
  match g.get2D! p' with
  | '#' => none
  | '.' => g.set2D! p '.' |>.set2D! p' v |> Option.some
  | 'O' =>
     let g <- g.move p' d
     g.set2D! p '.' |>.set2D! p' v |> Option.some
  | _ => none
  
def Grid.score (g: Grid) := Id.run $ do
  let mut score := 0
  for c in [0:g.size,0:g[0]!.size] do
     if g.get2D! c == 'O' then
        score := score + c.fst * 100 + c.snd
  return score

def Grid.expand (g: Grid) : Grid :=
   g.map (fun row =>
      row.flatMap (match · with
        | '#' => #['#','#']
        | '.' => #['.','.']
        | 'O' => #['[',']']
        | '@' => #['@','.']
        | _ => #[]
      ))

partial def Grid.moveExpanded (g: Grid) (p: Coord) (d: Direction) : Option Grid := do
  let p' <- d.move p
  let v := g.get2D! p
  match g.get2D! p' with
  | '#' => none
  | '.' => g.set2D! p '.' |>.set2D! p' v |> Option.some
  | '[' | ']' =>
     match d with
     | .Up | .Down =>
        let (neigbourDirection, neigbourChar) :=
           if g.get2D! p' == '['
           then (Direction.Right, ']')
           else (Direction.Left, '[')
        let p'neigbour <- neigbourDirection.move p'
        let hasNeigbour := g.get2D! p'neigbour == neigbourChar
        let g <- g.moveExpanded p' d
        let g <-
            if hasNeigbour
            then g.moveExpanded p'neigbour d
            else some g
        g.set2D! p '.' |>.set2D! p' v |> Option.some
     | _ =>
        let g <- g.moveExpanded p' d
        g.set2D! p '.' |>.set2D! p' v |> Option.some
  | _ => none

def Grid.scoreExpanded (g: Grid) := Id.run $ do
  let mut score := 0
  for c in [0:g.size,0:g[0]!.size] do
     if g.get2D! c == '[' then
        score := score + c.fst * 100 + c.snd
  return score


def process (input: String) :=
   let data := input.splitOn "\n\n"
   let g : Grid := data[0]!.toGrid
   let directions := data[1]!.toList.filterMap Direction.ofChar?
   let p := g.find2D? (· == '@') |>.get!
   flip directions.foldl (g,p) (fun (g,p) d =>
      if let some g := g.move p d
      then (g, d.move p |>.getD p)
      else (g, p)
   )
   |>.fst
   |>.score


#example process testInput evaluates to 10092
#example process <$> input evaluates to 1495147
 
def testInput2 := "#######
#...#.#
#.....#
#..OO@#
#..O..#
#.....#
#######

<vv<<^^<<^^"

def process' (input: String) : Nat := 
   let data := input.splitOn "\n\n"
   let g : Grid := data[0]!.toGrid
   let directions := data[1]!.toList.filterMap Direction.ofChar?
   let g := g.expand
   let p := g.find2D? (· == '@') |>.get!
   flip directions.foldl (g,p) (fun ((g,p): (Grid × Coord)) d => 
      if let some (g: Grid) := g.moveExpanded p d
      then (g, d.move p |>.getD p)
      else (g, p)
   ) 
   |>.fst
   |>.scoreExpanded



#example process' testInput evaluates to 9021
#example process' <$> input evaluates to 1524905

