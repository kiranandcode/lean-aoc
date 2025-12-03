import Batteries

namespace String
-- hehehe
instance : GetElem String Nat Char (fun xs i => i < xs.length) where
   getElem := fun s ind _ => String.Pos.Raw.get s (Pos.Raw.mk ind)

def concat (s : List String) (sepBy := "") : String :=
   s.foldl (fun acc v => if acc.isEmpty then v else acc ++ sepBy ++ v) ""


def splitLines (s: String) : List String :=
   s.splitOn "\n"
   |>.map (·.trim)
   |>.filter (not ∘ String.isEmpty)

def words (s: String) : List String :=
  s.trim
  |>.splitOn " "
  |>.filter (not ∘ String.isEmpty)

def toGrid (s: String) : Array (Array Char) :=
   s.splitOn "\n"
   |>.filter (not ∘ String.isEmpty)
   |>.toArray.map (List.toArray ∘ String.toList)

end String

