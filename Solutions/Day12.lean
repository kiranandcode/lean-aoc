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

--        (-1,0)
--  (0, -1)      (0, 1)
--        (1, 0)

abbrev HSet A [BEq A] [Hashable A] := Std.HashSet A
abbrev HMap A B [BEq A] [Hashable A] := Std.HashMap A B
abbrev Grid := Array (Array Char)
abbrev Coord := Nat × Nat
abbrev ICoord := Int × Int

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

def Std.HashMap.first? [Hashable A] [BEq A] (map: HMap A B) : Option A := 
   match map.foldM (fun _ (k: A) _ => .error k) () with 
   | Except.ok () => none
   | Except.error e => e

def Std.HashMap.first! [Inhabited A] [Hashable A] [BEq A] (map: HMap A B) : A :=
   map.first?.get!

abbrev PCoordMap := HMap ICoord Nat

def visualise (pcoords: PCoordMap) : IO Unit := do
   let coords := pcoords.keys
   let ys := coords.map (·.fst) 
   let xs := coords.map (·.snd)
   let ymin := ys.min?.get!
   let ymax := ys.max?.get!
   let xmin := xs.min?.get!
   let xmax := xs.max?.get!
   let ywidth := ymax - ymin + 1
   let xwidth := xmax - xmin + 1
   let mut arr := Array.mkArray ywidth.toNat (Array.mkArray xwidth.toNat ' ')
   for j in [0:ywidth.toNat] do
      for i in [0:xwidth.toNat] do
         let coord := ((j : Int) + ymin, (i: Int) + xmin)
         if let some n := pcoords[coord]? then
             arr := arr.set2D! ((coord.fst - ymin).toNat, (coord.snd - xmin).toNat)
                (Char.ofNat ('0'.toNat + n))
   for row in arr do
      println! "  {row.toList}"
   return

def computeNoSides (perimeterCoords : PCoordMap) (region: HSet Coord) : IO Nat := do
  let region := region.toList.map Coord.toICoord |> Std.HashSet.ofList
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
  let mut coordDirs : HMap ICoord (HSet ICoord) := .empty

  while !perimeterCoords.isEmpty do
     println! "at start of iteration {noSides}:"
     visualise perimeterCoords
     -- retrieve a random coord on the perimeter
     let coord := perimeterCoords.first!
     perimeterCoords := dropCoord perimeterCoords coord
     noSides := noSides + 1
     -- find neighbour that is in perimeter coords, plus we have not considered this direction
     let inRegionCoord :=
       neigbours coord
       |>.find? (fun neigbour =>
          let dir := neigbour.sub coord |>.swap
          region.contains neigbour
          && !(coordDirs[coord]?.get?.contains dir
              || coordDirs[coord]?.get?.contains dir.negate)
          && (perimeterCoords.contains (coord.add dir) ||
              perimeterCoords.contains (coord.add dir.negate)))
     if let some inRegionCoord := inRegionCoord then
        -- store direction to dir
        let regionDir := inRegionCoord.sub coord
        -- work out direction of this path
        let dir := regionDir  |>.swap
        coordDirs := coordDirs.update coord (·.get?.insert dir)
        -- println! "at coord {coord} chose direction {dir}"
        let revdir := dir.negate
        let mut nextCoord := coord.add dir
        -- repeatedly remove the coords in this direction
        while perimeterCoords.contains nextCoord && region.contains (nextCoord.add regionDir) do
            perimeterCoords := dropCoord perimeterCoords nextCoord
            coordDirs := coordDirs.update nextCoord (·.get?.insert dir)
            nextCoord := nextCoord.add dir
        nextCoord := coord.add revdir
        -- repeatedly remove the coords in this direction
        while perimeterCoords.contains nextCoord && region.contains (nextCoord.add regionDir) do
            perimeterCoords := dropCoord perimeterCoords nextCoord
            coordDirs := coordDirs.update nextCoord (·.get?.insert dir)
            nextCoord := nextCoord.add revdir

  return noSides


def text :=
"XXXXX
X..X.
XX.X.
X..XX
XXXXX"

--  22222
-- 3XXXXX
-- 3X  XX
-- 3XX  X
-- 3X44XX
-- 3XXXXX
--  11111

#eval
  let g := text.toGrid
  let regions := computeRegions g
  let pCoords := computePerimeterCoords regions[0]!.snd
  computeNoSides pCoords regions[0]!.snd

def process' (input : String) : IO Nat := do
  let g := input.toGrid
  let regions := computeRegions g

  let res <- regions.mapM (fun (r: Char × HSet Coord) => do
       let area := r.snd.size
       let pCoords := computePerimeterCoords r.snd
       let noSides <- computeNoSides pCoords r.snd
       -- println! "A region of {r.fst} plants with price {area} * {noSides} = {area * noSides}"
       return area * noSides
     )
  return res |>.foldl Nat.add 0


def testInput3 := "AAAAAA
AAABBA
AAABBA
ABBAAA
ABBAAA
AAAAAA"

-- #example process' testInput evaluates to 80
-- #eval do let i <- input; process' i -- evaluates to 1206
-- 954835
-- #example process' testInput3 evaluates to 368
-- #example process' <$> input evaluates to 922682

