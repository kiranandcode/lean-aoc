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

def allPartitions (ls : List A) : List (List (List A)) :=
   match ls with
   | [] => [[]]
   | (h :: t) =>
   t.allPartitions.flatMap (match · with
   | [] => [[h] :: []]
   | h' :: t' => [(h :: h') :: t', [h] :: h' :: t'])


@[specialize]
def foldl2  {α : Type u} {β : Type v} (f : α → (β × β) → α) : (init : α) → List β → α
  | a, nil | a, cons _ nil  => a
  | a, cons b1 (cons b2 l) => foldl2 f (f a (b1,b2)) l

@[specialize]
def foldl2D  {α : Type u} {β : Type v} [Inhabited β] (f : α → (β × β) → α) : (init : α) → List β → α
  | a, nil => a
  | a, cons b1 nil  => f a (b1, default)
  | a, cons b1 (cons b2 l) => foldl2D f (f a (b1,b2)) l


@[specialize]
protected def foldl2M {m : Type u → Type v} [Monad m] {s : Type u} {α : Type w} : (f : s → (α × α) → m s) → (init : s) → List α → m s
  | _, s, []      | _, s, _ :: []      => pure s
  | f, s, a1 :: a2 :: as => do
    let s' ← f s (a1,a2)
    List.foldl2M f s' as


private def takeFindInternal (p : α → Bool) (acc: List α) : List α → Option (α × List α)
| [] => none
| a :: l => if p a then some (a, acc.reverseAux l) else takeFindInternal p (acc.cons a) l

def takeFind? (p : α → Bool) : List α → Option (α × List α) :=
  takeFindInternal p []

structure Take2 (A: Type) where data : List A
def take2 (ls: List A) : Take2 A := ⟨ls⟩

end List

#eval [1,2,3,4].takeFind? (· == 3)


instance [Monad M] : ForIn M (List.Take2 A) (A × A) where
   forIn := fun a b f => do
      let rec loop b ls := match ls with
         | [] => return b
         | _ :: [] => return b
         | a1 :: a2 :: rest => do
             let res <- (f (a1,a2) b)
             match res with
             | .done v => return v
             | .yield b =>
                 loop b rest
      loop b a.data

#eval
  Id.run $ do
    let mut acc := #[]
    for (a,b) in [1,2,3,4,5].take2 do
      acc := (acc.push (a,b))
    return acc

def List.Take2.toList (t: List.Take2 A) : List (A × A) := Id.run $ do
    let mut ls := #[]
    for (l,r) in t do
       ls := ls.push (l,r)
    return ls.toList
       

private def List.insertSortedTR (ls : List A) (leq: A -> A -> Bool) (x: A) (acc: List A) : List A :=
  match ls with
  | [] => acc.reverseAux [x]
  | h :: t => if leq x h then acc.reverseAux (x :: h :: t) else t.insertSortedTR leq x (h :: acc)

def List.insertSorted (leq: A -> A -> Bool) (x: A) (ls : List A) : List A :=
   ls.insertSortedTR leq x []

@[inline]
def List.flatMapM  {M: Type -> Type}  [Monad M]
   (a : List α) (b : α → M (List β)) : M (List β) := do
   Functor.map flatten (mapM b a)
