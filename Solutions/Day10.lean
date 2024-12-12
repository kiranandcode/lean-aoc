import Batteries
import Utils

def input := AOC.getInput 10

def testInput := "89010123
78121874
87430965
96549874
45678903
32019012
01329801
10456732"

abbrev Coord := Nat × Nat
abbrev Grid := Array (Array Nat)


partial def noReachableNines (g: Grid) (start: Coord) := Id.run $ do
   let mut ninesFound := Std.HashSet.empty
   let mut visited := Std.HashSet.empty
   let mut queue := #[start]
   while !queue.isEmpty do
      let elt := queue.back!
      queue := queue.pop
      let eltVl := g.get2D! elt
      if eltVl == 9 then
         ninesFound := ninesFound.insert elt
         continue
      if visited.contains elt then
         continue
      visited := visited.insert elt
      for neighbour in (g.neigboursOf elt) do
         let neighbourVl := g.get2D! neighbour
         if neighbourVl == eltVl + 1 then
             queue := queue.push neighbour

   return ninesFound.size

def process (input: String): Nat := Id.run $ do
   let g := input.toGrid
            |>.map (Array.map Char.toDigit)
   let mut zeroes := #[]
   let mut nines := #[]
   
   for i in [0:g.size, 0:g[0]!.size] do
      let iVl := g.get2D! i
      if iVl == 0 then
         zeroes := zeroes.push i
      if iVl == 9 then
         nines := nines.push i
   let mut scores := 0
   for zero in zeroes do
      let trailHeadScore := noReachableNines g zero
      scores := scores + trailHeadScore
   return scores
   
#example process testInput evaluates to 36
#example process <$> input evaluates to 510

partial def noDistinctReachableNines (g: Grid) (start: Coord) := Id.run $ do
   let mut ninesFound := Std.HashMap.empty
   let mut queue := #[start]
   while !queue.isEmpty do
      let elt := queue.back!
      queue := queue.pop
      let eltVl := g.get2D! elt
      if eltVl == 9 then
         ninesFound := ninesFound.update elt (·.get? + 1)
         continue
      for neighbour in g.neigboursOf elt do
         let neighbourVl := g.get2D! neighbour
         if neighbourVl == eltVl + 1 then
             queue := queue.push neighbour

   return ninesFound.values.sum

def process' (input: String) := Id.run $ do
   let g := input.toGrid
            |>.map (Array.map Char.toDigit)
   let mut zeroes := #[]
   let mut nines := #[]
   
   for i in [0:g.size, 0:g[0]!.size] do
      let iVl := g.get2D! i
      if iVl == 0 then
         zeroes := zeroes.push i
      if iVl == 9 then
         nines := nines.push i
   let mut scores := 0
   for zero in zeroes do
      let trailHeadScore := noDistinctReachableNines g zero
      scores := scores + trailHeadScore
   return scores


#example process' testInput evaluates to 81
#example process' <$> input evaluates to 1058
