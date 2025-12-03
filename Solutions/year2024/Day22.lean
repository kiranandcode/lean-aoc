import Batteries
import Utils

abbrev HMap A B [BEq A] [Hashable A] := Std.HashMap A B
abbrev HSet A [BEq A] [Hashable A] := Std.HashSet A

def testInput := "1
10
100
2024"

def testInput2 := "1
2
3
2024"

def input := AOC.getInput 22 (year := .some 2025)

abbrev Integer := UInt64

def Integer.ofNat (n: Nat) : Integer := UInt64.ofNat n

def Integer.mix (m1 m2: Integer) : Integer:=
  m1.xor m2

def Integer.prune (m1: Integer) : Integer :=
   m1.mod 16777216

#eval Integer.mix 42 15
#eval Integer.prune 100000000

def step (secret: Integer) : Integer :=
   let secret := secret.mix (secret * 64) |>.prune
   let secret := secret.mix (secret / 32) |>.prune
   let secret := secret.mix (secret * 2048) |>.prune
   secret



#example List.iterate step 123 10
evaluates to ([15887950, 16495136, 527345, 704524,
 1553684, 12683156, 11100544, 12249484, 7753432, 5908254]: List Integer)


def process (input: String) :=
 input.splitLines
      |>.map (Integer.ofNat ∘ String.toNat!)
      |>.map (Function.iterate step 2000)
      |>.sum

#example process testInput evaluates to (37327623: Integer)
-- #eval process <$> input
   
def computeDiffs (ls: List Int) :=
   let hd := ls.head!
   ls.tail!.foldMap (fun prev v =>
    ((v, v - prev), v)
   ) hd
   |>.fst

def buildMap (map: HMap (List Int) Int)  (ls : List (Int × Int)) := Id.run $ do
   let mut map := map
   for sequence in ls.slidingWindow 4 do
      let pattern := sequence.map Prod.snd
      let score := sequence.getLast!.fst
      map := map.update pattern (·.getD score)
   return map

def merge (maps: List (HMap (List Int) Int)) : HMap (List Int) (List Int) := 
   maps.foldl (fun bmap map =>
      map.keys.foldl (fun bmap k =>
          bmap.update k (·.get?.cons map[k]!)
      ) bmap
   ) .empty 

def process' (input: String) :=
      input.splitLines
      |>.map (Integer.ofNat ∘ String.toNat!)
      |>.map (fun v => (List.iterate step v 2000).cons v)
      |>.map (List.map (fun v => (v.mod 10 |>.toNat |> Int.ofNat)))
      |>.map computeDiffs
      |>.map (buildMap .empty)
      |> merge
      |>.toList
      |>.map (Prod.map id List.sum)
      |>.mergeSort (fun a b => a.snd >= b.snd)
      |>.head!
      |>.snd

#example process' testInput2 evaluates to (23: Int)
-- #eval process' <$> input

def main: IO Unit := do
   let res <- process' <$> input
   println! "{res}"
