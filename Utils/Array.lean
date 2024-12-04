import Batteries
import Utils.Macros
import Utils.Option

structure Array.Diagonals (A: Type) where arr : Array (Array A)
structure Array.RevDiagonals (A: Type) where arr : Array (Array A)


instance [Inhabited A] [Monad M] : ForIn M (Array.Diagonals A) (List A) where
  forIn := fun (⟨a⟩) b f =>  do
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

#example (runST $ fun _ => do
  let mut ls := ([]: List (List Nat))
  for i in (#[#[ 1, 2, 3, 4],
              #[ 5, 6, 7, 8],
              #[ 9,10,11,12],
              #[13,14,15,16]]).revDiagonalArrs do
    ls := i :: ls
  return ls.reverse) evaluates to (
  [[16],
   [12, 15],
   [8, 11, 14],
   [4, 7, 10, 13],
   [3, 6, 9],
   [2, 5],
   [1]] : List (List Nat))


