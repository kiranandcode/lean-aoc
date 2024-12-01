import Batteries

namespace String
def concat (s : List String) (sepBy := "") : String :=
   s.foldl (fun acc v => acc ++ sepBy ++ v) ""


def splitLines (s: String) : List String :=
   s.splitOn "\n"
   |>.map (·.trim)
   |>.filter (not ∘ String.isEmpty)

end String

