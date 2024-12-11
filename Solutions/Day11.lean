import Utils
import Batteries

def input := AOC.getInput 11

def testInput := "0 1 10 99 999"

abbrev Stone := Nat

def update (s: Stone) :=
   if s == 0
   then [1]
   else if s.noDigits % 2 == 0
   then let (l,r) := s.splitDigits; [l,r]
   else [s * 2024]

def process (input: String) :=
  let ls := input.words.map String.toNat!
  List.range 25 |>.foldl (fun (ls : List Nat) _ => ls.flatMap update) ls
  |>.length


#example process testInput evaluates to 125681
#example process <$> input evaluates to 194482

abbrev Cache := Std.HashMap Stone (List Stone)

def cachedUpdate' (cache: Cache) (currentSet: Std.HashSet Stone) := Id.run $ do
  let mut cache := cache
  let mut currentSet := currentSet
  let mut newSet := Std.HashSet.empty
  for elt in currentSet do
    match cache[elt]? with
    | some v => newSet := newSet.insertMany v
    | none =>
       let res := update elt
       cache := cache.insert elt res
       newSet := newSet.insertMany res
  return (newSet, cache)

abbrev SizeCache := Std.HashMap (Nat × Stone) Nat

-- cached compute the size of a subtree at depth {depth}
partial def computeSize (depth: Nat) (cache : Cache) (sizeCache: SizeCache) (elt: Stone) : (Nat × SizeCache) :=
    if depth == 0
    then (1, sizeCache.insert (depth, elt) 1)
    else match sizeCache.get? (depth, elt) with
    | none => Id.run $ do
        let mut size := 0
        let mut sizeCache := sizeCache
        for subElt in cache.get? elt |>.get? do
            let res := computeSize (depth - 1) cache sizeCache subElt
            size := size + res.fst
            sizeCache := res.snd
        sizeCache := sizeCache.insertIfNew (depth, elt) size
        return (size, sizeCache)
    | some res => (res, sizeCache)


def process' (n: Nat) (input: String) : IO Nat := do
  let ls := input.words.map String.toNat!
  let mut currentSet := Std.HashSet.ofList ls
  let mut cache := Std.HashMap.empty
  -- first build a graph of stone -> newStone
  for _ in [0:n] do
     let res := cachedUpdate' cache currentSet
     currentSet := res.fst
     cache := res.snd

  let mut sizeCache := Std.HashMap.empty
  let mut size := 0

  for elt in ls do
     let res := computeSize n cache sizeCache elt
     size := size + res.fst
     sizeCache := res.snd
  return size


def main: IO Unit := do
   let input <- input
   let res <- process' 75 input
   println! "final result: {res}"

