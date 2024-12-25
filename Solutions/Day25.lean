import Batteries
import Utils

def input := AOC.getInput 25
def testInput := "#####
.####
.####
.####
.#.#.
.#...
.....

#####
##.##
.#.##
...##
...#.
...#.
.....

.....
#....
#....
#...#
#.#.#
#.###
#####

.....
.....
#.#..
###..
###.#
###.#
#####

.....
.....
.....
#....
#.#..
#.#.#
#####"

def parseInput (input: String) : List (Array Nat) × List (Array Nat) :=
   let (locks, keys) :=
       input.splitOn "\n\n"
       |>.map String.toGrid
       |>.map Array.transpose
       |>.partition (fun k => k[0]![0]! == '#')
   let locks :=
      locks.map (fun k => k.map (fun r => r.takeWhile (· == '#') |>.size |> (· - 1)))
   let keys :=
      keys.map (fun k => k.map (fun r => r.takeWhile (· == '.') |>.size |> (Array.size r - · - 1)))
   (locks,keys)      


def lockFitsKey (lock: Array Nat) (key: Array Nat) :=
   lock.zip key |>.map (Function.uncurry Nat.add) |>.all (· < 6)

def process (input: String) :=
   let (locks, keys) := parseInput input
   locks.product keys |>.countP (Function.uncurry lockFitsKey)


#eval process testInput
#eval process <$> input

