import Batteries

namespace Std
structure RangeProd where
  fst : Range
  snd : Range


namespace RangeProd

@[inline] protected def forIn {m} [Monad m] 
  (range: RangeProd) (b: β) (f: Nat × Nat → β → m (ForInStep β)) : m β := do
  let mut b := b
  for i in range.fst do
     for j in range.snd do
        let res <- f (i,j) b
        match res with
        | .done b' => return b'
        | .yield b' => b := b'
  return b
        
end RangeProd

instance  : ForIn m RangeProd (Nat × Nat) where
  forIn := RangeProd.forIn


syntax:max "[" withoutPosition(term ":" term) ", " withoutPosition(term ":" term) "]" : term
syntax:max "[" withoutPosition(term ":" term ":" term)  ", " withoutPosition(term ":" term ":" term) "]" : term

macro_rules
  | `([ $start1 : $stop1 , $start2 : $stop2 ]) =>
      `({fst := { start := $start1, stop := $stop1 : Range },
         snd := { start := $start2, stop := $stop2 : Range }
      : RangeProd})
  | `([ $start1 : $stop1 : $step1, $start2 : $stop2 : $step2 ]) =>
      `({fst := { start := $start1, stop := $stop1, step := $step1 : Range },
         snd := { start := $start2, stop := $stop2, step := $step2 : Range }
      : RangeProd})

end Std


#eval (do
   for i in [0:10, 1:20] do
      println! "{i}"
   : IO Unit)
