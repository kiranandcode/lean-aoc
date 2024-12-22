import Batteries
import Utils.Coord

inductive Direction where
| Up
| Down
| Left
| Right
deriving Inhabited, BEq, Hashable, Repr

instance : ToString Direction where
   toString
   | .Up => "Up"
   | .Left => "Left"
   | .Down => "Down"
   | .Right => "Right"

def Direction.turnRight : Direction -> Direction
| .Up => .Right
| .Down => .Left
| .Left => .Up
| .Right => .Down

def Direction.turnLeft : Direction -> Direction
| .Up => .Left
| .Down => .Right
| .Left => .Down
| .Right => .Up

def Direction.turnAround : Direction -> Direction
| .Up => .Down
| .Down => .Up
| .Left => .Right
| .Right => .Left

def Directions : List Direction := [.Up, .Down, .Left, .Right]

def Direction.ofChar? : Char -> Option Direction
| '^' => .some .Up
| '>' => .some .Right
| '<' => .some .Left
| 'v' => .some .Down
| _ => .none

def Direction.toICoord : Direction -> ICoord
| .Up => (-1,0)
| .Down => (1,0)
| .Left => (0,-1)
| .Right => (0,1)

def Direction.move (coord: Coord) : Direction -> Option Coord
| .Up => if coord.fst > 0 then some (coord.fst-1,coord.snd) else none
| .Down => some (coord.fst+1,coord.snd)
| .Left => if coord.snd > 0 then some (coord.fst,coord.snd - 1) else none
| .Right => some (coord.fst,coord.snd + 1)

def Direction.moveBy (by_: Nat) (coord: Coord) : Direction -> Option Coord
| .Up => if coord.fst >= by_ then some (coord.fst-by_,coord.snd) else none
| .Down => some (coord.fst+by_,coord.snd)
| .Left => if coord.snd >= by_ then some (coord.fst,coord.snd - by_) else none
| .Right => some (coord.fst,coord.snd + by_)

