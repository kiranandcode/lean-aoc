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


def process (bound: Int) (input: String) :=
  let g := input.toGrid
  let sPos := g.find2D? (· == 'S') |>.get!
  let ePos := g.find2D? (· == 'E') |>.get!
  let scores := buildScores g sPos ePos
  scores.keys.flatMap (computeSavings g scores)
  |>.map Prod.snd
  |>.filter (· >= bound)
  |>.length

#example process 1 testInput evaluates to 44
#example process 100 <$> input evaluates to 1321

def computeAllCheats (scores: HMap Coord Int) (coords: List Coord) := Id.run $ do
  let mut cheats := Std.HashMap.empty
  for c1 in coords do
     for c2 in coords do
         let distance := c1.manhattenDistance c2
         if distance <= 20 && scores[c1]! > scores[c2]! + distance then
            cheats := cheats.insert (c1,c2) (scores[c1]! - scores[c2]! - distance)
  return cheats

def process' (bound: Int) (input: String) :=
  let g := input.toGrid
  let sPos := g.find2D? (· == 'S') |>.get!
  let ePos := g.find2D? (· == 'E') |>.get!
  let scores := buildScores g sPos ePos
  let cheats := computeAllCheats scores scores.keys
  cheats.values
  |>.filter (· >= bound)
  |>.length

#example process' 50 testInput evaluates to 285
-- #example process' 100 <$> input evaluates to 971737


def main : IO Unit := do
  let res <- process' 100 <$> input
  println! "{res}"
-- 971737
