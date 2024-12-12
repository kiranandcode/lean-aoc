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

def Coord.toICoord (pos: Coord) : ICoord := 
   ((pos.fst : Int), (pos.snd : Int))


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

def HashMap.first? [Hashable A] [BEq A] (map: HMap A B) := Id.run $ do
  try (
     map.foldM (fun k v => do
       throw k
     )
  ) catch k => k

abbrev PCoordMap := HMap ICoord Nat
def computeNoSides (perimeterCoords : PCoordMap) := Id.run $ do
  -- okay, here's the plan, we have this list of all coords on the perimeter
  -- we're going to iteratively remove them until no remain
  let mut noSides := 0
  let mut perimeterCoords := perimeterCoords
  -- remove coord from the map
  let dropCoord (map : PCoordMap) (coord: ICoord) :=
     let map := map.update coord (·.get? - 1)
     if map[coord]?.get? == 0
     then map.erase coord
     else map

  while !perimeterCoords.isEmpty do
     let coord := 
