import Batteries
import Utils

def input := AOC.getInput 19 (year := .some 2025)

def testInput := "r, wr, b, g, bwu, rb, gb, br

brwrr
bggr
gbbr
rrbgbr
ubwu
bwurrg
brgr
bbrgwb"

abbrev HMap A B [Hashable A] [BEq A] := Std.HashMap A B
abbrev HSet A [Hashable A] [BEq A] := Std.HashSet A

structure PrefixTrie where
   nextStateId : Nat
   doneStates : HSet Nat
   nextState : HMap Nat (HMap Char Nat)
deriving Repr

def PrefixTrie.empty : PrefixTrie := { nextStateId := 1, doneStates := default, nextState := default}
instance : Inhabited PrefixTrie where
   default := { nextStateId := 1, doneStates := default, nextState := default}

def PrefixTrie.getOrCreateNextState (currentState : Nat) (c: Char) (t: PrefixTrie) : (Nat × PrefixTrie) :=
   match t.nextState[currentState]?.get?.get? c with
   | .none =>
      let nextStateId := t.nextStateId
      let nextState := t.nextState.update currentState (fun v => v.get?.insert c nextStateId)
      (nextStateId, {t with nextStateId := t.nextStateId + 1, nextState})
   | .some id => (id, t)
      
def PrefixTrie.getNextState (currentState : Nat) (c: Char) (t: PrefixTrie) : Option Nat :=
   t.nextState[currentState]?.get?.get? c


partial def PrefixTrie.insertInternal (currentState: Nat) (s: String) (pos: Nat) (t: PrefixTrie) :=
   if pos >= s.length
   then {t with doneStates := t.doneStates.insert currentState}
   else
      let c := s.get! (String.Pos.mk pos)
      let (currentState, t) := t.getOrCreateNextState currentState c
      t.insertInternal currentState s (pos + 1)

def PrefixTrie.finalState? (state: Nat) (t: PrefixTrie) :=
    t.doneStates.contains state

def PrefixTrie.insert (s: String) (t: PrefixTrie) :=
    t.insertInternal 0 s 0

def step (t: PrefixTrie) (currentState: Nat) (c: Char) : List Nat :=
   t.getNextState currentState c
   |>.toList
   |>.flatMap (fun p => if t.finalState? p then [0,p] else [p])

def run (t: PrefixTrie) (s: String) :=
   s.toList.foldl (fun (s: HSet Nat) (c: Char) =>
       s.toList.flatMap (step t · c)
       |> Std.HashSet.ofList
   ) (Std.HashSet.empty.insert 0)
   |>.toList
   |>.filter t.finalState?

def process (input: String) :=
  let inputs := input.splitOn "\n\n"
  let pats := inputs[0]!.splitOn ", "
  let input := inputs[1]!.splitLines
  let trie :=
     pats.foldl (fun (t: PrefixTrie) (s: String) =>
        t.insert s) .empty
  List.countP (not ∘ List.isEmpty ∘ run trie) input


#example process testInput evaluates to 6
#example process <$> input evaluates to 228

def runAll (t: PrefixTrie) (s: String) :=
   s.toList.foldl (fun (s: HMap Nat Nat) (c: Char) =>
       s.toList
       |>.flatMap (fun (s, count) => step t s c |>.map (fun s => (s,count)))
       |>.foldl (fun (s: HMap Nat Nat) (st, cnt) =>
           s.update st (fun v => v.get? + cnt)
       ) .empty
   ) (Std.HashMap.empty.insert 0 1)
   |>.toList
   |>.filter (t.finalState? ∘ Prod.fst)

def process' (input: String) :=
  let inputs := input.splitOn "\n\n"
  let pats := inputs[0]!.splitOn ", "
  let input := inputs[1]!.splitLines
  let trie :=
     pats.foldl (fun (t: PrefixTrie) (s: String) =>
        t.insert s) .empty
  input.flatMap (List.map Prod.snd ∘ runAll trie)
  |>.sum

#example process' testInput evaluates to 16
#example process' <$> input evaluates to 584553405070389
