import Batteries

def Function.iterate {A: Type} (f: A -> A) (n: Nat) (init: A) :  A := match n with
| 0 => init
| .succ n => iterate f n (f init)
