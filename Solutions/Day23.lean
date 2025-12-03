import Batteries
import Utils

def input := AOC.getInput 23 (year := .some 2025)
def testInput := "kh-tc
qp-kh
de-cg
ka-co
yn-aq
qp-ub
cg-tb
vc-aq
tb-ka
wh-tc
yn-cg
kh-ub
ta-co
de-co
tc-td
tb-wq
wh-td
ta-ka
td-qp
aq-cg
wq-ub
ub-vc
de-ta
wq-aq
wq-vc
wh-yn
ka-de
kh-ta
co-tc
wh-qp
tb-vc
td-yn"

def parseInput (input: String) := 
    input.splitLines
    |>.map (String.splitOn (sep := "-"))
    |>.flatMap (fun ls => [(ls[0]!, ls[1]!), (ls[1]!, ls[0]!)])
    |>.toHashMapMulti
    |>.map (fun _ v => Std.HashSet.ofList v)

def process (input: String) := Id.run $ do
  let connections := parseInput input
  let mut results := Std.HashSet.empty
  for (k1, k1Neigbours) in connections do
      if !k1.startsWith "t" then
         continue
      for k2 in k1Neigbours do
         for k3 in connections[k2]! do
            if k1Neigbours.contains k3 then
               results := results.insert [k1,k2,k3].mergeSort
  return results.size


#example process testInput evaluates to 7
#example process <$> input evaluates to 926

def NeigbourScore := String × Nat
deriving Inhabited
instance : Max NeigbourScore where
   max l r := if l.snd > r.snd then l else r


def scoreNeigbour (key: String) (connections: Std.HashMap String (Std.HashSet String)) : String -> NeigbourScore :=
  (fun neigbour =>
     (neigbour, connections[neigbour]!.toList.countP (connections[key]!.contains ·))
  )

def process' (input: String) :=
  let connections := parseInput input
  connections.keys
  |>.map (fun key =>
     connections[key]!.toList.map (scoreNeigbour key connections)
     |>.max?
     |>.get!
     |> (key, ·.fst)
  )
  |>.map (fun (fst, snd) => Id.run $ do
     let mut elts : Std.HashSet String := {fst,snd}
     for neigbour in connections[fst]! do
        if elts.all (connections[·]!.contains neigbour) then
          elts := elts.insert neigbour
     return (elts.size, elts.toList.mergeSort)
  )
  |>.mergeSort (fun a b => a.fst >= b.fst)
  |>.head!
  |> Prod.snd
  |> String.concat (sepBy := ",")


#example process' testInput evaluates to "co,de,ka,ta"
#example process' <$> input evaluates to "az,ed,hz,it,ld,nh,pc,td,ty,ux,wc,yg,zz"
