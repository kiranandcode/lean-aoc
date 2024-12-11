import Batteries

namespace Nat

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

end Nat

