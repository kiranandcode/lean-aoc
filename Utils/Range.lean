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

structure RevRange where
  range : Range

namespace RevRange

theorem exists_mul_add_of_mod {n k b : Nat} (h : k % n = b) :
  ∃ a, n * a + b = k := by
  -- from mathlib: k % n + n * (k / n) = k
  have h₀ := Nat.mod_add_div k n
  -- rewrite remainder
  rw [h] at h₀
  -- rearrange
  rcases h₀.symm with h₁
  refine ⟨k / n, ?_⟩
  -- h₁: k = b + n * (k / n)
  -- goal: n * (k / n) + b = k
  omega

@[inline] partial def forIn [Monad m] (rrange : RevRange) (init : β)
    (f : Nat → β → m (ForInStep β)) : m β :=
  let range := rrange.range
  let rec @[specialize] loop (b : β) (i : Nat) (hu : i < range.stop) : m β := do
    match (← f i b) with
    | .done b  => pure b
    | .yield b =>
      if hpos : range.start + range.step ≤ i then
        loop b (i - range.step) (by omega)
      else
        pure b
  if hnonempty : range.start < range.stop then
    let offset := (range.stop - range.start - 1) / range.step * range.step
    let lastAligned := range.start + offset
    have hoffset_lt : offset < range.stop - range.start := by
      have := range.step_pos
      calc offset = (range.stop - range.start - 1) / range.step * range.step := rfl
        _ ≤ range.stop - range.start - 1 := Nat.div_mul_le_self _ _
        _ < range.stop - range.start := by omega
    loop init lastAligned (by omega)
  else
    pure init

end RevRange

instance : ForIn m RevRange Nat where
   forIn := RevRange.forIn

namespace Range
def rev (r: Range) := RevRange.mk r
end Range

end Std


