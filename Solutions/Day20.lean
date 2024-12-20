import Batteries
import Utils

def input := AOC.getInput 20

def testInput :=
"###############
#...#...#.....#
#.#.#.#.#.###.#
#S#...#.#.#...#
#######.#.#.###
#######.#.#...#
#######.#.###.#
###..E#...#...#
###.#######.###
#...###...#...#
#.#####.#.###.#
#.#...#.#.#...#
#.#.#.#.#.#.###
#...#...#...###
###############"

abbrev Grid := Array (Array Char)
abbrev HMap A B [Hashable A] [BEq A] := Std.HashMap A B
abbrev HSet A [Hashable A] [BEq A] := Std.HashSet A

def buildScores (g: Grid) (sPos: Coord) (ePos : Coord) : HMap Coord Int := Id.run $ do
    let mut map : HMap Coord Int := Std.HashMap.empty.insert ePos 0
    let mut cost := 0
    let mut coord := ePos
    while coord != sPos do
      let nextCoord := g.neigboursOf coord |>.filter (g.get2D! · != '#') |>.find? (not ∘ map.contains) |>.get!
      cost := cost + 1
      coord := nextCoord
      map := map.insert coord cost
    return map

def computeSavings (g: Grid) (costs: HMap Coord Int) (coord: Coord) :=
   g.neigboursOf coord
   |>.filter (g.get2D! · == '#')
   |>.flatMap (fun c => g.neigboursOf c
                        |>.filter (g.get2D! · != '#')
                        |>.filter (· != coord))
   |>.map (fun c => ((coord, c), costs[coord]! - costs[c]! - 2))


def process (input: String) :=
  let g := input.toGrid
  let sPos := g.find2D? (· == 'S') |>.get!
  let ePos := g.find2D? (· == 'E') |>.get!
  let scores := buildScores g sPos ePos
  scores.keys.flatMap (computeSavings g scores)
  |>.map Prod.snd
  |>.filter (· >= 100)
  |>.length

#eval process <$> input

def Int.max (v1 v2: Int) : Int := if v1 >= v2 then v1 else v2


def computeSavingsLonger (g: Grid) (costs: HMap Coord Int) (savings: HMap (Coord × Coord) Int) (startCoord: Coord) :
  IO (HMap (Coord × Coord) Int) := do
   let mut savings : HMap (Coord × Coord) Int := savings
   let mut step := 0
   let mut coords : HSet Coord := g.neigboursOf startCoord
                 |>.filter (g.get2D! · == '#')
                 |> Std.HashSet.ofList
   while step < 21 do
      step := step + 1
      let mut coords' := Std.HashSet.empty
      for coord in coords do
         let neigbours := g.neigboursOf coord |>.filter (· != startCoord)
         for neigbour in neigbours do
           if g.get2D! neigbour != '#' then
               let cost := (costs[startCoord]! - costs[neigbour]! - step - 1)
               savings := savings.update (startCoord, neigbour) (·.getD cost |>.max cost)
           else
              coords' := coords'.insert neigbour
      coords := coords'
   return savings


def main : IO Unit := do
  let g := testInput.toGrid
  let sPos := g.find2D? (· == 'S') |>.get!
  let ePos := g.find2D? (· == 'E') |>.get!
  let scores := buildScores g sPos ePos
  let res <-
     scores.keys.foldlM (computeSavingsLonger g scores) .empty
  let res := res
     |>.toList
     |>.filter (·.snd == 50)
     |>.map Prod.fst

  for (i, (s,e)) in res.enum do
     let g' := g.set2D! s 'X' |>.set2D! e 'Y'
     println! "\n\ncheat {i} from {s} to {e}:"
     println! "{g.visualise}"


