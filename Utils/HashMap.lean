import Batteries


namespace Std.HashMap
  variable {α : Type u} {β : Type v} {_ : BEq α} {_ : Hashable α}

  def update (h: HashMap α β) (k: α) (f: Option β -> β) : HashMap α β :=
      h.alter k (fun v => some (f v))

end Std.HashMap
