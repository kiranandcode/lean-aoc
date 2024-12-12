import Batteries

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
