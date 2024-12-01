import Utils.String
import Utils.Date
import Utils.File
import Utils.Network
import Utils.Macros

namespace AOC
   def getInput (day: Int) (year : Option Int := .none) := AOC.Network.getInput day (year := year)
end AOC
