import Batteries
import Utils


def input := AOC.getInput 8

def testInput := "............
........0...
.....0......
.......0....
....0.......
......A.....
............
............
........A...
.........A..
............
............"

abbrev Grid := Array (Array Char)
abbrev Coord := Int × Int

def Int.sqrt (n: Int) : Int := 
   if n < 0
   then - n.natAbs.sqrt
   else n.natAbs.sqrt

def Coord.x (v: Coord) := Prod.fst v
def Coord.y (v: Coord) := Prod.snd v

def Coord.sub (v1: Coord) (v2: Coord) : Coord := (v1.x - v2.x, v1.y - v2.y)
def Coord.add (v1: Coord) (v2: Coord) : Coord := (v1.x + v2.x, v1.y + v2.y)

def Coord.antinode (v1: Coord) (v2: Coord) :=
   let d := v2.sub v1
   v2.add d

def inBounds (g: Grid) (c: Coord) :=
   c.x >= 0 && c.y >= 0 && c.y < g.size && c.x < g[0]!.size

def isEmpty (g: Grid) (c: Coord) :=
   g[c.y.natAbs]![c.x.natAbs]! == '.'

def getAntinodes (g: Grid) (p1: Coord) (p2: Coord) :=
   [p1.antinode p2, p2.antinode p1]
   |>.filter (inBounds g)
   -- |>.filter (isEmpty g)


def extractAntennae (g: Grid)  : Std.HashMap Char (List (Int × Int)) := Id.run $ do
  let mut antennae := .empty
  for j in [0:g.size] do
     for i in [0:g[0]!.size] do
          if g[j]![i]! != '.' then
             antennae := antennae.update g[j]![i]! (·.get?.cons (i,j))
  return antennae

def process (input: String) : Nat := Id.run $ do
  let g := input.toGrid
  let mut antennae := extractAntennae g
  let mut antinodes : Std.HashSet Coord := Std.HashSet.empty
  for (_, coords) in antennae do
     let coords := coords.toArray
     for i in [0:coords.size] do
        for j in [i+1:coords.size] do
           antinodes := antinodes.insertMany (getAntinodes g coords[i]! coords[j]!)
  return antinodes.size

#eval getAntinodes testInput.toGrid (5,2) (7,3)


#example process testInput evaluates to 14
#example process <$> input evaluates to 320


-- yes could compute this with maths, but loops are easier
partial def getAllAntinodes (g: Grid) (p1: Coord) (p2: Coord) := Id.run $ do
   let mut antiNodes := #[p1, p2]
   let diff := p2.sub p1
   let mut current := p2.add diff
   while inBounds g current do
      antiNodes := antiNodes.push current
      current := current.add diff

   let diff := p1.sub p2
   current := p1.add diff
   while inBounds g current do
      antiNodes := antiNodes.push current
      current := current.add diff

   return antiNodes.toList

def process' (input: String) : Nat := Id.run $ do
  let g := input.toGrid
  let mut antennae := extractAntennae g
  let mut antinodes : Std.HashSet Coord := Std.HashSet.empty
  for (_, coords) in antennae do
     let coords := coords.toArray
     for i in [0:coords.size] do
        for j in [i+1:coords.size] do
           antinodes := antinodes.insertMany (getAllAntinodes g coords[i]! coords[j]!)
  return antinodes.size

#example process' testInput evaluates to 34
#example process' <$> input evaluates to 1157

