import Batteries
import Utils.Macros
import Utils.Option
import Utils.Coord
import Utils.String

structure Array.Diagonals (A: Type) where arr : Array (Array A)
structure Array.RevDiagonals (A: Type) where arr : Array (Array A)

instance [Inhabited A] [Monad M] : ForIn M (Array.Diagonals A) (List A) where
  forIn := fun ⟨a⟩ b f =>  do
  let h := a.size
  let w := a[0]?.map Array.size |>.getD 0
  let mut b := b

  for sjinv in [1:h] do
     let sj := h - sjinv
     let mut ls := []

     for i in [0:(h - sj).min (w - sj)] do
       ls := ls.cons (a[sj + i]?.get?[i]?.get?)

     match <- f ls.reverse b with
     | .yield b' => b := b'
     | .done b' => return b'


  for si in [0:w] do
     let mut ls := []

     for j in [0:(h - si).min (w - si)] do
       ls := ls.cons (a[j]?.get?[si + j]?.get?)

     match <- f ls.reverse b with
     | .yield b' => b := b'
     | .done b' => return b'

  return b

instance [Inhabited A] [Monad M] : ForIn M (Array.RevDiagonals A) (List A) where
  forIn := fun (⟨a⟩) b f =>  do
  let h := a.size
  let w := a[0]?.map Array.size |>.getD 0
  let mut b := b

  for sjinv in [1:h] do
     let sj := h - sjinv
     let mut ls := []
     let hi := (h - sj).min (w - sj)
     for iinv in [1:hi + 1] do
       let i := w - iinv
       ls := ls.cons (a[sj + iinv - 1]?.get?[i]?.get?)

     match <- f ls.reverse b with
     | .yield b' => b := b'
     | .done b' => return b'


  for siinv in [1:w+1] do
     let si := w - siinv
     let mut ls := []
     let hj := h.min (si + 1)
     for j in [0:hj] do
       ls := ls.cons (a[j]?.get?[si - j]?.get?)

     match <- f ls.reverse b with
     | .yield b' => b := b'
     | .done b' => return b'

  return b


def Array.diagonalArrs (arr: Array (Array A)) : Array.Diagonals A :=  Array.Diagonals.mk arr
def Array.revDiagonalArrs (arr: Array (Array A)) : Array.RevDiagonals A :=  Array.RevDiagonals.mk arr

def Array.diagonals [Inhabited A] (arr: Array (Array A)) : List (List A) := Id.run $ do
   let mut ls := []
   for diagonal in arr.diagonalArrs do
      ls := ls.cons diagonal
   return ls.reverse
      
def Array.revDiagonals [Inhabited A] (arr: Array (Array A)) : List (List A) := Id.run $ do
   let mut ls := []
   for diagonal in arr.revDiagonalArrs do
      ls := ls.cons diagonal
   return ls.reverse

def Array.get2D! [Inhabited A] (arr: Array (Array A)) (i: Nat × Nat) : A :=
   arr[i.fst]![i.snd]!

def Array.set2D! [Inhabited A] (arr: Array (Array A)) (i: Nat × Nat) (v: A) : Array (Array A) :=
    arr.set! i.fst (arr[i.fst]!.set! i.snd v)

def Array.get2D? (arr: Array (Array A)) (i: Nat × Nat) : Option A := do
   (<- arr[i.fst]?)[i.snd]?

def Array.find2D? (arr: Array (Array A)) (f: A -> Bool) : Option (Nat × Nat) := do
   let mut i := 0
   for row in arr do
      let mut j := 0
      for elt in row do
         if f elt then
            return (i,j)
         j := j + 1
      i := i + 1
   none

#example (Id.run $ do
  let mut ls := ([]: List (List Nat))
  for i in ((#[#[ 1, 2, 3, 4],
              #[ 5, 6, 7, 8],
              #[ 9,10,11,12],
              #[13,14,15,16]]) : Array (Array Nat)).revDiagonalArrs do
       ls := i :: ls
  return (ls.reverse : List (List Nat))) evaluates to (
  [[16],
   [12, 15],
   [8, 11, 14],
   [4, 7, 10, 13],
   [3, 6, 9],
   [2, 5],
   [1]] : List (List Nat))


def Array.neigboursOf (g: Array (Array A)) (pos : Nat × Nat) : List (Nat × Nat):=
    (if pos.fst > 0 then [(pos.fst - 1, pos.snd)] else []) ++
    (if pos.snd > 0 then [(pos.fst, pos.snd - 1)] else []) ++
    [(pos.fst + 1, pos.snd), (pos.fst, pos.snd + 1)]
    |>.filter (fun pos => pos.fst < g.size && pos.snd < g[0]!.size)

def Array.inBounds (g: Array (Array A)) (c: Coord) :=
   c.x >= 0 && c.y >= 0 && c.x < g.size && c.y < g[0]!.size

def Array.visualise (g: Array (Array Char)) :=
  String.concat (sepBy := "\n") (g.toList.map (fun row => String.ofList row.toList))

def Array.transpose [Inhabited A] (g: Array (Array A)) : Array (Array A) := Id.run $ do
  let w := g[0]!.size
  let mut res := #[]
  for i in [0:w] do
    res := res.push (g.map (fun r => r[i]!))
  return res


