import Batteries

namespace Int
  def isPos (i: Int) : Bool := i > 0
  def isNeg (i: Int) : Bool := i < 0

  def inRange (i: Int) (low: Int) (high: Int) : Bool :=
      low <= i && i <= high
end Int
