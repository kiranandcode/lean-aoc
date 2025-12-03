import Batteries
import Utils

def input : IO String := AOC.getInput 4 (year:=.some 2024)

def countXmas (s: String) :=
  let s := s.toLower
  (s.findAllSubstr "xmas".toSubstring |>.size) +
  (s.findAllSubstr "samx".toSubstring |>.size) 


def process (s: String) :=
  let linesH := s.splitLines
  let linesV :=
      linesH.map String.toList
      |>.transpose
      |>.map List.asString

  let sArray := linesH.toArray.map (List.toArray ∘ String.toList)
  let diagonalsF := sArray.diagonals.map List.asString
  let diagonalsB := sArray.revDiagonals.map List.asString

           
  (linesH.map countXmas |>.sum) +
  (linesV.map countXmas |>.sum) +
  (diagonalsF.map countXmas |>.sum) +
  (diagonalsB.map countXmas |>.sum)
  
  
#example process "..X...
.SAMX.
.A..A.
XMAS.S
.X...." evaluates to 3

def exampleInput : String := "MMMSXXMASM
MSAMXMSMSA
AMXSXMAAMM
MSAMASMSMX
XMASAMXAMM
XXAMMXXAMA
SMSMSASXSS
SAXAMASAAA
MAMMMXMMMM
MXMXAXMASX"

#example process exampleInput evaluates to 18
#example process <$> input evaluates to 2524


def process' (s: String) : Nat := Id.run $ do
  let g := s.toGrid
  let h := g.size
  let w := g[0]!.size
  let get i j := g[j]![i]!

  let checkPatternAt i j : Bool := Id.run $ do
     if !(i + 2 < w && j + 2 < h) then
       return false
     ((get i j == 'M' && get (i + 1) (j + 1) == 'A' && get (i + 2) (j + 2) == 'S')
     || (get i j == 'S' && get (i + 1) (j + 1) == 'A' && get (i + 2) (j + 2) == 'M')) &&
     ((get (i + 2) j == 'M' && get (i + 1) (j + 1) == 'A' && get i (j + 2) == 'S')
     || (get (i + 2) j == 'S' && get (i + 1) (j + 1) == 'A' && get i (j + 2) == 'M'))

  let mut count := 0
  for i in [0:w] do
    for j in [0:h] do
       if checkPatternAt i j then
          count := count + 1
  return count


#example process' exampleInput evaluates to 9
#example process' <$> input evaluates to 1873
