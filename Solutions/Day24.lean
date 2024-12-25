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
  | .And => "and"
  | .Or => "or"
  | .Xor => "xor"

def Op.ofString! : String -> Op
| "AND" => And
| "OR" => Or
| "XOR" => Xor
| _ => panic! "unexpected op!"

def parseInstruction (s: String) : (Op × String × String) :=
   let words := s.words
   (Op.ofString! words[1]!, words[0]!, words[2]!)

structure Machine where
  inputs: HMap String Bool
  rules: HMap String (Op × String × String)
deriving Repr

def Machine.ofString (input: String) : Machine :=
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
      Machine.mk inputs computations

def Machine.propagate (m: Machine) : Machine := Id.run $ do
      let mut inputs := m.inputs
      let mut remaining := Std.HashSet.ofList m.rules.keys
      let mut anyChanges := true
      while !remaining.isEmpty && anyChanges do
          anyChanges := false
          for elt in remaining do
              let (op, v1, v2) := m.rules[elt]!
              if inputs.contains v1 && inputs.contains v2 then
                  anyChanges := true
                  inputs := inputs.insert elt (op.apply inputs[v1]! inputs[v2]!)
                  remaining := remaining.erase elt
      return {m with inputs}

#eval (4).toBvec

def Machine.getXsize (m: Machine) : Nat :=
  m.inputs.keys.filter (·.startsWith "x") |>.length
def Machine.getYsize (m: Machine) : Nat :=
  m.inputs.keys.filter (·.startsWith "y") |>.length
def Machine.getZsize (m: Machine) : Nat :=
  m.rules.keys.filter (·.startsWith "z") |>.length

def Machine.getXvec (m: Machine) : List (String × Bool) :=
    m.inputs.toList
    |>.filter (flip String.startsWith "x" ∘ Prod.fst)
    |>.mergeSort (·.fst <= ·.fst)

def Machine.getYvec (m: Machine) : List (String × Bool) :=
    m.inputs.toList
    |>.filter (flip String.startsWith "y" ∘ Prod.fst)
    |>.mergeSort (·.fst <= ·.fst)

def Machine.getZvec (m: Machine) : List String :=
    m.rules.toList
    |>.map Prod.fst
    |>.filter (flip String.startsWith "z")
    |>.mergeSort

def Machine.getInternalVec (m: Machine) : List (String) :=
    m.rules.toList
    |>.map Prod.fst
    |>.filter (not ∘ String.startsWith (pre:="z"))
    |>.mergeSort


def Machine.setX (x: Nat) (m: Machine) : Machine := Id.run $ do
   let mut inputs := m.inputs
   let xkeys := m.inputs.keys.filter (·.startsWith "x") |>.mergeSort
   for xinput in xkeys do
      inputs := inputs.insert xinput false
   for (xinput, vl) in xkeys.zip x.toBvec do
      inputs := inputs.insert xinput vl
   {m with inputs}

def Machine.setY (y: Nat) (m: Machine) : Machine := Id.run $ do
   let mut inputs := m.inputs
   let xkeys := m.inputs.keys.filter (·.startsWith "y") |>.mergeSort
   for yinput in xkeys do
      inputs := inputs.insert yinput false
   for (yinput, vl) in xkeys.zip y.toBvec do
      inputs := inputs.insert yinput vl
   {m with inputs}

def Machine.getZ (m: Machine) : Nat :=
    m.getZvec
    |>.map (m.inputs.get!)
    |> Nat.ofBvec

def Machine.getZ? (m: Machine) : Option Nat :=
    m.getZvec
    |>.map (m.inputs.get?)
    |>.allSome
    |> Option.map Nat.ofBvec

def Machine.getDeps (output: String) (m: Machine) : HSet String := Id.run $ do
   let mut deps : HSet String := .empty
   let mut queue := [output]
   let mut anyChanges := true
   while anyChanges do
      anyChanges := false
      let mut newQueue := #[]
      for elt in queue do
          if let some (_, l, r) := m.rules[elt]? then
              newQueue := newQueue.push l
              newQueue := newQueue.push r
              anyChanges := true
      deps := deps.insertMany newQueue
      queue := newQueue.toList
   return deps

def Machine.run (x y: Nat) (m: Machine) : Nat :=
    m.setX x
    |>.setY y
    |>.propagate
    |>.getZ

def Machine.run? (x y: Nat) (m: Machine) : Option Nat :=
    m.setX x
    |>.setY y
    |>.propagate
    |>.getZ?


def Machine.swap (o1 o2: String) (m: Machine) : Option Machine :=
  if (m.getDeps o1 |>.contains o2) || (m.getDeps o2 |>.contains o1)
  then none
  else some {m with rules:=m.rules
            |>.insert o2 m.rules[o1]!
            |>.insert o1 m.rules[o2]!}
def Machine.swap! (o1 o2: String) (m: Machine) :  Machine :=
 {m with rules:=m.rules
            |>.insert o2 m.rules[o1]!
            |>.insert o1 m.rules[o2]!}


def Machine.getZwrong (expt: Nat) (m: Machine) :=
   m.getZvec.zip expt.toBvec
   |>.filter (fun (pair: String × Bool) => m.inputs[pair.fst]! != pair.snd)

def Machine.depsOf (output: String) (m: Machine) : HSet String :=
   match m.rules[output]? with
   | .none => {}
   | .some (_, l, r) => {l,r}

def Machine.applySwaps (ls: List (String × String)) (m: Machine) : Machine :=
      ls.foldl (fun (m: Machine) (l,r) => m.swap! l r) m

def process (input: String) :=
  let machine := Machine.ofString input
  machine
  |>.propagate
  |>.getZ

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

def IO.withStdoutToString [Monad m] [MonadFinally m] [MonadLiftT BaseIO m] (x : m α) : m (String × α) := open IO in open FS in do
  let bOut ← mkRef { : Stream.Buffer }
  let r ← withStdout (Stream.ofBuffer bOut) x
  let bOut ← liftM (m := BaseIO) bOut.get
  let out := String.fromUTF8! bOut.data
  pure (out, r)

   
def toBvexpr (sz: Nat) (elts: List String) :=
     elts.enum.map
        (fun ((i: Nat), (k: String)) =>
          s!"(bvshl (ite {k} (_ bv{1} {sz}) (_ bv{0} {sz})) (_ bv{i} {sz}))" 
        )
        |> String.concat (sepBy:=" ")
        |> (fun v => s!"(bvor {v})")

def buildSelectorExpr (i: Nat) :=
    List.range 4
    |>.foldl (fun (s: String) ind =>
      s!"(ite (= {i} swap-{ind}-in)
              swap-{ind}-out
              (ite (= {i} swap-{ind}-out)
                   swap-{ind}-in
                   {s}))"
    ) s!"{i}"

def getExpr (iter: Nat) (internalNodeMap: HMap String Nat) (v: String) (selector: Bool) :=
   if v.startsWith "x" || v.startsWith "y"
   then s!"{v}-{iter}"
   else if selector
   then s!"(select internal-{iter} {buildSelectorExpr internalNodeMap[v]!})"
   else s!"(select internal-{iter} {internalNodeMap[v]!})"

def declareModelAdditionConstraints (iter: Nat) (m: Machine) (assert: String -> IO Unit) (x: Nat) (y: Nat) : IO Unit := do
  let sz := m.getZsize
  println!"(declare-const internal-{iter} (Array Int Bool))"
  for (x,_) in m.getXvec do
     println!"(declare-const {x}-{iter} Bool)"
  for (y,_) in m.getYvec do
     println!"(declare-const {y}-{iter} Bool)"
  let internalNodeMap := (m.getInternalVec ++ m.getZvec).enum.map Prod.swap |>.toHashMap

  for (output, op, v1, v2) in m.rules do
     assert s!"(=
                 {getExpr iter internalNodeMap output (selector:=false)}
                 ({op}
                    {getExpr iter internalNodeMap v1 true}
                    {getExpr iter internalNodeMap v2 true}))"

  let xExpr := m.getXvec.map Prod.fst |>.map (fun v => s!"{v}-{iter}") |> toBvexpr sz
  let yExpr := m.getYvec.map Prod.fst |>.map (fun v => s!"{v}-{iter}") |> toBvexpr sz
  let zExpr := m.getZvec |>.map (fun v => getExpr iter internalNodeMap v true) |> toBvexpr sz

  assert s!"(= (bv2int {xExpr}) {x})"
  assert s!"(= (bv2int {yExpr}) {y})"

  assert s!"(= {zExpr} (bvadd {xExpr} {yExpr}))"

def main: IO Unit := do
   let m := Machine.ofString (<- input)
   let internalNodeRevMap := (m.getInternalVec ++ m.getZvec).enum |>.toHashMap

   let sz := m.getZsize
   let mut nConstraints := [ ]
   let mut foundSuccess := false
   let mut finalSwaps : List (String × String) := []
   let mut inputPairs := [ (2^(sz -1) - 1, 2^(sz -1) - 1) ]
   
   while !foundSuccess do
      let (buf, ()) <- IO.withStdoutToString $ do
         let aid <- IO.mkRef 0
         let assert s : IO Unit := do
            let idv <- aid.get
            println! "(assert (! {s} :named a{idv}))"
            aid.set (idv + 1)

         for i in [0:4] do
            println! "(declare-const swap-{i}-in Int)"
            println! "(declare-const swap-{i}-out Int)"
         for nConstraint in nConstraints do
             assert <|
                nConstraint.take2.toList.enum.flatMap (fun (i, (l,r)) =>
                 [s!"(= swap-{i}-in {l})",
                  s!"(= swap-{i}-out {r})",]
                )
                |> String.concat (sepBy := " ")
                |> (fun v => s!"(not (and {v}))")
 
         for i in [0:4] do
           assert s!"(not (= swap-{i}-in swap-{i}-out))"
           assert s!"(< swap-{i}-in {internalNodeRevMap.size})"
           assert s!"(< swap-{i}-out {internalNodeRevMap.size})"
           assert s!"(<= 0 swap-{i}-in)"
           assert s!"(<= 0 swap-{i}-out)"
           for j in [0:4] do
              if i != j then
                assert s!"(not (= swap-{i}-in swap-{j}-in))"
                assert s!"(not (= swap-{i}-in swap-{j}-out))"
                assert s!"(not (= swap-{i}-out swap-{j}-out))"
                assert s!"(not (= swap-{j}-in swap-{i}-out))"

         for (i,(l,r)) in inputPairs.enum do
           declareModelAdditionConstraints i m assert l r 

         println! "(check-sat)"
         for i in [0:4] do
            println! "(eval swap-{i}-in)"
            println! "(eval swap-{i}-out)"

      if <- ("/tmp/day24.z3" : System.FilePath).pathExists then
         IO.FS.removeFile "/tmp/day24.z3";
      IO.FS.writeFile "/tmp/day24.z3" buf
      println! "generated query; running z3"
      let res <- IO.Process.runCmdWithInput "z3" #["-smt2", "/tmp/day24.z3"]
      let res := res.splitLines
      let rawRes := res.tail!.map String.toNat!
      let swaps := if res.head! == "sat"
         then some (rawRes |>.map (internalNodeRevMap.get!) |>.take2.toList)
         else none
      match swaps with
      | none => println! "reached unsat :("; break
      | some swaps =>
         println! "found candidate swaps {swaps}!"
         foundSuccess := true
         let m' := m.applySwaps swaps
         for (l,r) in inputPairs do
            let res := m'.run? l r
            if res.isNone then
               foundSuccess := false
               println! "machine does not produce output for values {l},{r}"
               break
            if res.isSome && l + r != res.get! then
               foundSuccess := false
               println! "does not produce the expected output for {l} + {r} = {l + r} != {res.get!}"
               break
         if foundSuccess then
             for i in [0:3] do
                let l <- IO.rand 0 (2^(sz - 1) - 1)
                let r <- IO.rand 0 (2^(sz - 1) - 1)
                let res := m'.run? l r
                if res.isNone then
                   foundSuccess := false
                   println! "machine does not produce output for values {l},{r}"
                   break
                if res.isSome && l + r != res.get! then
                   inputPairs := inputPairs.cons (l,r)
                   foundSuccess := false
                   println! "does not produce the expected output for {l} + {r} = {l + r} != {res.get!}, adding as a pair"
                   break


         if !foundSuccess then
           println! "not the solution, cegis and continue"
           nConstraints := nConstraints.cons rawRes
         else
           finalSwaps := swaps
      println! "{finalSwaps.flatMap (fun (a,b) => [a,b]) |>.mergeSort |>String.concat (sepBy:=",")}"

