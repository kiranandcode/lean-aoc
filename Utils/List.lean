import Batteries
import Utils.HashMap

namespace List

def toHashMap {K V} [BEq K] [Hashable K] (l: List (K × V)) : Std.HashMap K V :=
    flip l.foldl Std.HashMap.empty
      (fun map m => map.insert m.fst m.snd)

def toHashMapMulti {K V} [BEq K] [Hashable K] (l: List (K × V)) : Std.HashMap K (List V) :=
    flip l.foldl Std.HashMap.empty
      (fun map m => map.update m.fst (·.getD [] |>.cons m.snd))


def foldMap (f : A -> B -> C × A) (init: A) (ls: List B) : List C × A := Id.run $ do
    let mut res := []
    let mut acc := init
    for elt in ls do
       let (v, acc') := f acc elt
       res := res.cons v
       acc := acc'
    return (res.reverse, acc)

def foldMapM [Monad M] (f : A -> B -> M (C × A)) (init: A) (ls: List B) : M (List C × A) := do
    let mut res := []
    let mut acc := init
    for elt in ls do
       let (v, acc') <- f acc elt
       res := res.cons v
       acc := acc'
    return (res.reverse, acc)

def midElement [Inhabited A] (ls: List A) : A :=
   ls[ls.length/2]?.get?

end List
