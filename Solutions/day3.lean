import Batteries
import Utils
open Std

def input := AOC.getInput 3

def testInput := "987654321111111
811111111111119
234234234234278
818181911112111"

-- lics i k = longest increasing subsequence length gt k in first i cells
-- lics i 0 = max (a[i]) (lics (i - 1) 0)
   -- lics len gt 0 in first i cells is the largest cell
-- lics i k | i < k = 0
   -- lics len gt k in less than i cells is 0 
-- lics i k =  max (lics (i - 1) (k - 1) * 10 + a[i]) (lics (i - 1) k)
   -- lics len gt k is max of
      -- lics len gt (k - 1) * 10 + a [i] (i.e lics including current element)
      -- lics (i - 1) k

def lics (maxSz: Nat) (arr: Array Nat)  := Id.run $ do
   let mut lics :=
       Array.replicate arr.size
          (Array.replicate maxSz 0)

   lics := lics.set2D! (0, 0) arr[0]!
   for i in [1:arr.size] do
      lics := lics.set2D! (i, 0)
          (Nat.max
             (arr[i]!)
             (lics.get2D! (i - 1, 0)))

   for k in [1:maxSz] do
       for i in [k:arr.size] do
          lics := lics.set2D! (i,k) $
              Nat.max
                (lics.get2D! (i - 1, k))
                (lics.get2D! (i - 1, k - 1) * 10 + arr[i]!)
    lics.get2D! (arr.size - 1, maxSz - 1)

def processInput (input: String) :=
      input.splitLines
      |>.map (Array.mk ∘ String.toList)
      |>.map (Array.map Char.toDigit)

def part1 (input: String) :=
      processInput input
      |>.map (lics 2)
      |>.sum

def part2 (input: String) :=
      processInput input
      |>.map (lics 12)
      |>.sum

#example part1 testInput evaluates to 357
#example part2 testInput evaluates to 3121910778619

#example part1 <$> input evaluates to 17430
#example part2 <$> input evaluates to 171975854269367

