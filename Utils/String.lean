import Batteries

namespace String
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
   s.splitLines
   |>.toArray.map (List.toArray ∘ String.toList)

end String

