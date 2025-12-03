import Batteries
import Utils.List

structure UF (A: Type) [BEq A] [Hashable A] where
   nextId: Nat
   map: Std.HashMap A Nat
   uf: Batteries.UnionFind


section UF
variable {A} [BEq A] [Hashable A]

def UF.empty : UF A :=
   {nextId:=0, map:=.emptyWithCapacity, uf:=.empty}

instance : Inhabited (UF A) where
  default := UF.empty


def UF.insert (v: A) (uf: UF A) : UF A :=
   if uf.map.contains v
   then uf
   else
     let map := uf.map.insert v (uf.nextId)
     {uf with map,nextId:=uf.nextId + 1, uf:=uf.uf.push}

def UF.addEq (a b: A) (uf: UF A) :=
   let uf := uf.insert a
             |>.insert b
   let aId := uf.map[a]!
   let bId := uf.map[b]!
   let (ufD, isEq) := uf.uf.checkEquiv! aId bId
   if isEq
   then {uf with uf:=ufD}
   else
      let ufD := uf.uf.union! aId bId
      {uf with uf:=ufD}

def UF.toHashSets (uf: UF A) : List (Std.HashSet A) :=
   uf.map.toList.foldMap (fun (ufD: Batteries.UnionFind) (k,id) =>
     let (ufD, id) := ufD.find! id
     ((id, k), ufD)
     ) uf.uf
   |>.fst
   |>.toHashMapMulti
   |>.values
   |>.map Std.HashSet.ofList
  
end UF
