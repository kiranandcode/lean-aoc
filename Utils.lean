import Utils.String
import Utils.Date
import Utils.File
import Utils.Network
import Utils.Macros
import Utils.HashMap
import Utils.HashSet
import Utils.Option
import Utils.Int
import Utils.Char
import Utils.Either
import Utils.Parsec
import Utils.Array
import Utils.List
import Utils.Nat
import Utils.Range
import Utils.Direction
import Utils.Coord
import Utils.Function

namespace AOC
   def getInput (day: Int) (year : Option Int := .none) := AOC.Network.getInput day (year := year)
end AOC
