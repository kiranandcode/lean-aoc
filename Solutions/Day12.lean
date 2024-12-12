import Batteries
import Utils

def input := AOC.getInput 12

def testInput := "AAAA
BBCD
BBCC
EEEC"

def testInput2 := "RRRRIICCFF
RRRRIICCCF
VVRRRCCFFF
VVRCCCJFFF
VVVVCJJCFE
VVIVCCJJEE
VVIIICJJEE
MIIIIIJJEE
MIIISIJEEE
MMMISSJEEE"

abbrev HSet A [BEq A] [Hashable A] := Std.HashSet A
abbrev HMap A B [BEq A] [Hashable A] := Std.HashMap A B
abbrev Grid := Array (Array Char)
abbrev Coord := Nat × Nat
abbrev ICoord := Int × Int

instance : Min ICoord where
  min p1 p2 :=
    if p1.fst < p2.fst || (p1.fst == p2.fst && p1.snd < p2.snd)
    then p1
    else p2

def Coord.toICoord (pos: Coord) : ICoord := 
   ((pos.fst : Int), (pos.snd : Int))

def ICoord.negate (p1: ICoord) : ICoord := 
   (- p1.fst, - p1.snd)

def ICoord.sub (p1: ICoord) (p2: ICoord) : ICoord := 
   (p1.fst - p2.fst, p1.snd - p2.snd)

def ICoord.add (p1: ICoord) (p2: ICoord) : ICoord := 
   (p1.fst + p2.fst, p1.snd + p2.snd)

def ICoord.swap (p1: ICoord) : ICoord :=
   (p1.snd, p1.fst)

def neigbours (pos: ICoord) : List ICoord :=
   [(pos.fst - 1, pos.snd), (pos.fst, pos.snd - 1),
    (pos.fst + 1, pos.snd), (pos.fst, pos.snd + 1)]


def computeRegionOf (g: Grid) (c: Coord) (seen: HSet Coord) := Id.run $ do
   let colour := g.get2D! c
   let mut queue := g.neigboursOf c
   let mut elts : HSet Coord := {c}
   let mut seen := seen

   while !queue.isEmpty do
      let elt := queue.head!
      queue := queue.tail!
      if seen.contains elt then
         continue
      seen := seen.insert elt
      if g.get2D! elt != colour then
         continue
      elts := elts.insert elt
      queue :=
        g.neigboursOf elt
        |>.filter (not $ elts.contains ·)
        |>.filter (not $ seen.contains ·)
        |> queue.append
  
   return elts   

def computePerimeterCoords  (region: HSet Coord) : HMap ICoord Nat :=
   -- going to do this in a dumb but easy way
   -- every point has a perimter of its neigbours
   -- take the union of all of those, subtract the region itself and voila
   let region : HSet ICoord := Std.HashSet.ofList <| region.toList.map Coord.toICoord
   let insert (acc: HMap ICoord Nat) (n : ICoord) := acc.update n (·.get? + 1)
   region.fold (fun (acc: HMap ICoord Nat) v =>
     neigbours v
     |>.filter (not $ region.contains ·)
     |>.foldl insert acc
   ) .empty

def computePerimeter (region : HSet Coord) : Nat :=
   computePerimeterCoords region
   |>.values
   |>.sum

def computeRegions (g: Grid) := Id.run $ do
  let mut regions := #[]
  let mut seen : HSet Coord := .empty

  for coord in [0:g.size, 0:g[0]!.size] do
     if seen.contains coord then
        continue
     let region := computeRegionOf g coord seen
     regions := regions.push (g.get2D! coord, region)
     seen := seen.union region
  return regions


def process (input : String) := Id.run $ do
  let g := input.toGrid
  let regions := computeRegions g

  return regions.map (fun (r: Char × HSet Coord) => 
       let area := r.snd.size
       let perimeter := computePerimeter r.snd
       area * perimeter
     )
     |>.foldl Nat.add 0

#example process testInput evaluates to 140 
#example process testInput2 evaluates to 1930
#example process <$> input evaluates to 1549354

abbrev PCoordMap := HMap ICoord Nat

def ICoord.addDirection (c: ICoord) : Direction -> ICoord
| .Up => (c.fst - 1, c.snd)
| .Down => (c.fst + 1, c.snd)
| .Left => (c.fst, c.snd - 1)
| .Right => (c.fst, c.snd + 1)

def computeNoSides (region : HSet Coord) := Id.run $ do
  let region := region.toList.map Coord.toICoord
  let mut outerCoords :=
    region.filter (fun v => neigbours v |>.any (not $ region.contains ·))
    |>.flatMap (fun v =>
       Directions.filter (fun d => not $ region.contains (v.addDirection d))
       |>.map (fun d => (v, d))
    )
    |> Std.HashSet.ofList
  let mut noSides := 0
  while !outerCoords.isEmpty do
     noSides := noSides + 1
     -- pick a coord on the outline and direction to edge
     let (coord, edir) := outerCoords.first!
     outerCoords := outerCoords.erase (coord, edir)
     -- extract directions to move
     let dir := edir.turnLeft
     let revdir := edir.turnRight

     -- first move left
     let mut nextCoord := coord.addDirection dir
     while outerCoords.contains (nextCoord, edir) do
        outerCoords := outerCoords.erase (nextCoord, edir)
        nextCoord := nextCoord.addDirection dir
     -- then move right
     nextCoord := coord.addDirection revdir
     while outerCoords.contains (nextCoord, edir) do
        outerCoords := outerCoords.erase (nextCoord, edir)
        nextCoord := nextCoord.addDirection revdir
     -- we have removed this side from the list of outercoords, move on
  
  return noSides

def process' (input : String) : Nat := 
  let g := input.toGrid
  let regions := computeRegions g

  regions.map (fun (r: Char × HSet Coord) => 
       r.snd.size * computeNoSides r.snd
     )
  |>.foldl Nat.add 0


def testInput3 := "AAAAAA
AAABBA
AAABBA
ABBAAA
ABBAAA
AAAAAA"

#example process' testInput evaluates to 80
#example process' <$> input evaluates to 937032
#example process' testInput3 evaluates to 368

