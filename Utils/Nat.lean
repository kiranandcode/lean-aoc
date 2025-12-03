import Batteries

namespace Nat

def factors (n: Nat) : List Nat := Id.run $ do
  let mut factors := #[]
  for i in 1 ... (n / 2) + 1 do
     if n % i == 0 then
        factors := factors.push i
  return factors.toList

def noDigits (n: Nat) := Id.run $ do
   let mut noDigits: Nat := 1
   let mut n := n
   while n >= 10 do
       n := n / 10
       noDigits := noDigits + 1
   return noDigits

-- combines n1 n2 by concatenating them literally
def combine (n1 n2: Nat) : Nat :=
   n1 * 10^(n2.noDigits) + n2

def splitDigits (n: Nat) : Nat × Nat :=
  let exp := 10^(n.noDigits / 2)
  let rhs := n / exp
  let lhs := n - rhs * exp
  (rhs, lhs)

-- computes a natural number from a list of bits from least to most
-- significant
def ofBvec (ls: List Bool) : Nat :=
    ls.reverse.foldl (fun n (v: Bool) => n * 2 + (if v then 1 else 0)) 0

private def natToBvecTR (n: Nat) (acc: List Bool) : List Bool :=
   if n < 2
   then acc.cons (n == 1) |>.reverse
   else natToBvecTR (n/2)  (acc.cons (n % 2 == 1)) 

-- returns the Bits of {n} from least to most significant 
def toBvec (n: Nat) : List Bool := natToBvecTR n []

end Nat

