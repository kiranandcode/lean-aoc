import Batteries

namespace Option

@[inline] def get? {α : Type u} [Inhabited α] : Option α → α
  | some x => x
  | none   => default

end Option
