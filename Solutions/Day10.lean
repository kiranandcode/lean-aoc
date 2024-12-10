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
abbrev Distances := Std.HashMap (Coord × Coord) Nat

def newDistance (distances: Distances) (i: Coord)  (j: Coord) (k: Coord) : Option Nat := 
    let ij := distances[(i,j)]?
    let ik := distances[(i,k)]?
    let kj := distances[(i,k)]?
    match ij, ik, kj with
    | some ij, some ik, some kj => if ik + kj < ij then some (ik + kj) else none
    | none, some ik, some kj => some (ik + kj)
    | _, _, _ => .none

#eval do
   let input := testInput
   let g := input.toGrid
            |>.map (Array.map Char.toDigit)
   let mut distances : Distances := .empty
   let mut zeroes := #[]
   let mut nines := #[]
   
   for i in [0:g.size, 0:g[0]!.size] do
      let iVl := g.get2D! i
      if iVl == 0 then
         zeroes := zeroes.push i
      if iVl == 9 then
         nines := nines.push i
      for j in [0:g.size, 0:g[0]!.size] do
          let jVl := g.get2D! j
          if iVl == jVl + 1 then
             distances := distances.insert (i,j) 1

   for k in [0:g.size, 0:g[0]!.size] do
     for i in [0:g.size, 0:g[0]!.size] do
        for j in [0:g.size, 0:g[0]!.size] do
          if let some dist := newDistance distances i j k then
            distances := distances.insert (i,j) dist
   let mut scores := 0
   for zero in zeroes do
      let mut trailHeadScore := 0
      for nine in nines do
         if distances.contains (nine, zero) then
           trailHeadScore := trailHeadScore + 1
      println! "score for trailhead {zero} is {trailHeadScore}"
      scores := scores + trailHeadScore
   return scores
   
