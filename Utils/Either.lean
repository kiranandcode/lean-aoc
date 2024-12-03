inductive Either (A: Type) (B: Type) where
| left (a: A)
| right (b: B)
deriving Inhabited, Repr, BEq

namespace Either

def mapLeft (f: A -> C) : Either A B -> Either C B
  | .left v => .left (f v)
  | .right v => .right v

def mapRight (f: A -> C) : Either B A -> Either B C
  | .left v => .left v
  | .right v => .right (f v)

end Either
