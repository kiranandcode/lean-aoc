import Batteries

namespace String
def concat (s : List String) (sepBy := "") : String :=
   s.foldl (fun acc v => acc ++ sepBy ++ v) ""
end String

