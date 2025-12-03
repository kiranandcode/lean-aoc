import Batteries
import Utils
open Std

def input := AOC.getInput 1
def testInput := "L68
L30
R48
L5
R60
L55
L1
L99
R14
L82"
set_option maxRecDepth 10000


def parseInput (input: String) :=
   input.splitLines
   |>.map (fun s =>
      ((s.drop 1 |>.toInt!) *
       (if s[0]! == 'L' then -1 else 1)))

def part1 (input: String)  :=
   parseInput input
   |>.scanl (a := (50: Int))
      (fun acc vl => (acc + vl) % 100)
   |>.countP (· == 0)

def split100s (n: Int) :=
   if n < -100
   then -100 :: split100s (n + 100)
   else if n > 100
   then 100 :: split100s (n - 100)
   else [n]
termination_by n.natAbs

def passesZero (angle: Int) (rotation: Int) : Bool :=
   angle != 0 && (
     (angle + rotation <= 0  ) ||
     (angle + rotation >= 100)
   ) || rotation.natAbs >= 100

def part2 (input: String)  :=
   parseInput input
   |>.flatMap split100s
   |>.foldMap (init := (50: Int))
      (fun acc rotation =>
        ((acc, rotation), (acc + rotation) % 100))
   |>.fst.countP (Function.uncurry passesZero)

#example part1 testInput
  evaluates to 3
#example part2 testInput
  evaluates to 6

#example (part1 <$> input)
evaluates to 1191

#example (part2 <$> input)
evaluates to 6858
