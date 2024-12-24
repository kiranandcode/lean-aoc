import Batteries
import Utils

def input := AOC.getInput 24

def testInput := "x00: 1
x01: 1
x02: 1
y00: 0
y01: 1
y02: 0

x00 AND y00 -> z00
x01 XOR y01 -> z01
x02 OR y02 -> z02"

abbrev HMap A B [Hashable A] [BEq A] := Std.HashMap A B
abbrev HSet A [Hashable A] [BEq A] := Std.HashSet A
abbrev IMap := HMap String Bool

inductive Op where | And | Or | Xor
deriving Repr, Inhabited

def Op.apply (l r: Bool) : Op -> Bool
| And => l && r
| Or => l || r
| Xor => l ^^ r

instance : ToString Op where
  toString
  | .And => "AND"
  | .Or => "OR"
  | .Xor => "XOR"

def Op.ofString! : String -> Op
| "AND" => And
| "OR" => Or
| "XOR" => Xor
| _ => panic! "unexpected op!"

def parseInstruction (s: String) : (Op × String × String) :=
   let words := s.words
   (Op.ofString! words[1]!, words[0]!, words[2]!)

def parseInput (input: String) :=
      let ls := input.splitOn "\n\n"
      let (inputs, bindings) := (ls[0]!, ls[1]!)
      let inputs :=
         inputs.splitLines
         |>.map (String.splitOn (sep:=":"))
         |>.map (fun v => (v[0]!, String.trim v[1]! |>.toNat! |> (fun v => (v > 0 : Bool))))
         |>.toHashMap
      let computations :=
         bindings.splitLines
         |>.map (String.splitOn (sep:="->"))
         |>.map (List.map String.trim)
         |>.map (fun v => (v[1]!, parseInstruction v[0]!))
         |>.toHashMap
     (inputs, computations)

def computeResults (inputs: HMap String Bool) (computations: HMap String (Op × String × String)) := Id.run $ do
      let mut inputs := inputs
      let mut remaining := Std.HashSet.ofList computations.keys
      while !remaining.isEmpty do
          for elt in remaining do
              let (op, v1, v2) := computations[elt]!
              if inputs.contains v1 && inputs.contains v2 then
                  inputs := inputs.insert elt (op.apply inputs[v1]! inputs[v2]!)
                  remaining := remaining.erase elt
      return inputs


def bvecToNat (ls: List Bool) : Nat :=
    ls.reverse.foldl (fun n (v: Bool) => n * 2 + (if v then 1 else 0)) 0

def natToBvecTR (n: Nat) (acc: List Bool) : List Bool :=
   if n < 2
   then acc.cons (n == 1) |>.reverse
   else natToBvecTR (n/2)  (acc.cons (n % 2 == 1)) 

def natToBvec (n: Nat) : List Bool := natToBvecTR n []


def process (input: String) :=
  let (inputs, computations) := parseInput input
  computeResults inputs computations
      |>.toList
      |>.filter (flip String.startsWith "z" ∘ Prod.fst)
      |>.mergeSort (·.fst <= ·.fst)
      |>.map Prod.snd
      |> bvecToNat

#example process testInput evaluates to 4
#example process <$> input evaluates to 51657025112326

def testInput2 := "x00: 0
x01: 1
x02: 0
x03: 1
x04: 0
x05: 1
y00: 0
y01: 0
y02: 1
y03: 1
y04: 0
y05: 1

x00 AND y00 -> z05
x01 AND y01 -> z02
x02 AND y02 -> z01
x03 AND y03 -> z03
x04 AND y04 -> z04
x05 AND y05 -> z00"

def toPadded (ind: Nat) : String :=
   if ind < 9 then "0" ++ s!"{ind + 1}" else s!"{ind + 1}"

def inv (computations: HMap String (Op × String × String)) (inputs: HMap String Bool) (config: String × Bool)
  : List (List (String × Bool))  :=
   let (output, expected) := config
   let (op, v1, v2) := computations[output]!
   match op, expected, inputs[v1]!, inputs[v2]! with
   | .And, true, v1E, v2E => (if v1E then [] else [[(v1,true)]]) ++ (if v2E then [] else [[(v2,true)]])
   | .And, false, v1E, v2E =>
      if v1E && v2E then [[(v1, false)], [(v2,false)], [(v1,false), (v2,false)]]
      else [[]]
   | .Or, false, v1E, v2E => (if !v1E then [] else [[(v1,false)]]) ++ (if !v2E then [] else [[(v2,false)]])
   | .Or, true, v1E, v2E =>
      if !v1E && !v2E then [[(v1, true)], [(v2, true)], [(v1,true), (v2,true)]]
      else [[]]
   | .Xor, true, v1E, v2E =>
      if v1E && v2E then [[(v1, false)], [(v2, false)]]
      else if !v1E && !v2E then [[(v1, true)], [(v2, true)]]
      else [[]]
   | .Xor, false, true, false => [[(v1, false)], [(v2, false)]]
   | .Xor, false, false, true => [[(v1, true)], [(v2, false)]]
   | .Xor, false, _, _ => []

def toMap (inputs: List (String × Bool)) : Option (List (String × Bool)) := do
   let mut hmap: HMap String Bool := .empty
   for (k,v) in inputs do
      if k.startsWith "x" || k.startsWith "y" then none
      match hmap[k]?.map (· == v) with
      | .none => hmap := hmap.insert k v
      | .some true => continue
      | .some false => none
   return hmap.toList
   
def main : IO Unit := do
  let (inputs, computations) := parseInput (<-input)
  let x :=
    inputs.toList
    |>.filter (flip String.startsWith "x" ∘ Prod.fst)
    |>.mergeSort (·.fst <= ·.fst)
    |>.map Prod.snd
    |> bvecToNat
  let y :=
    inputs.toList
    |>.filter (flip String.startsWith "y" ∘ Prod.fst)
    |>.mergeSort (·.fst <= ·.fst)
    |>.map Prod.snd
    |> bvecToNat
  let inputs := computeResults inputs computations
  let z : List Bool := x + y |> natToBvec
  let zInvalid :=  z
                   |>.enum
                   |>.map (fun (ind, (z: Bool)) => ("z" ++ toPadded ind, z))
                   |>.filter (fun ((ind: String), z) => inputs[ind]?.isSome && inputs[ind]! != z)
  for elt in  zInvalid
  |>.take 5
  |>.map (inv computations inputs)
  |>.combinations
  |>.map List.flatten
  |>.filterMap toMap
  |>.flatMap (List.flatten ∘ List.combinations ∘ List.map (inv computations inputs))
  |>.filterMap toMap
  |>.flatMap (List.flatten ∘ List.combinations ∘ List.map (inv computations inputs))
  |>.filterMap toMap
  |>.flatMap (List.flatten ∘ List.combinations ∘ List.map (inv computations inputs))
  |>.filterMap toMap
  |>.flatMap (List.flatten ∘ List.combinations ∘ List.map (inv computations inputs))
  |>.filterMap toMap
  |>.flatMap (List.map Prod.fst)
  |> Std.HashSet.ofList
  do
    println! "{elt}"

  println! "done!"
