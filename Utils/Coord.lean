import Batteries
import Utils.Parsec

def Coord := Nat × Nat
deriving Repr, BEq, Inhabited, Hashable

def ICoord := Int × Int
deriving Repr, BEq, Inhabited, Hashable

instance : Min ICoord where
  min p1 p2 :=
    if p1.fst < p2.fst || (p1.fst == p2.fst && p1.snd < p2.snd)
    then p1
    else p2

def Coord.sub (p1: Coord) (p2: Coord) : Option Coord :=
   if p1.fst > p2.fst && p1.snd > p2.snd
   then some (p1.fst - p2.fst, p1.snd - p2.snd)
   else none

def Coord.add (p1: Coord) (p2: Coord) : Coord := 
   (p1.fst + p2.fst, p1.snd + p2.snd)

def Coord.swap (p1: Coord) : Coord :=
   (p1.snd, p1.fst)

def Coord.x (c: Coord) : Nat := c.fst
def Coord.y (c: Coord) : Nat := c.snd

def Coord.toICoord (pos: Coord) : ICoord := 
   ((pos.fst : Int), (pos.snd : Int))

def ICoord.toCoord (pos: ICoord) : Coord := 
   (pos.fst.toNat, pos.snd.toNat)

def ICoord.negate (p1: ICoord) : ICoord := 
   (- p1.fst, - p1.snd)

def ICoord.mul (p1: ICoord) (v: Nat) : ICoord := 
   (p1.fst * v, p1.snd * v)

def ICoord.mod (p1: ICoord) (x: Int) (y: Int) : ICoord := 
   (p1.fst % x, p1.snd % y)

def ICoord.sub (p1: ICoord) (p2: ICoord) : ICoord := 
   (p1.fst - p2.fst, p1.snd - p2.snd)

def ICoord.add (p1: ICoord) (p2: ICoord) : ICoord := 
   (p1.fst + p2.fst, p1.snd + p2.snd)

def ICoord.swap (p1: ICoord) : ICoord :=
   (p1.snd, p1.fst)

def ICoord.x (c: ICoord) : Int := c.fst
def ICoord.y (c: ICoord) : Int := c.snd

def ICoord.neigbours (pos: ICoord) : List ICoord :=
   [(pos.fst - 1, pos.snd), (pos.fst, pos.snd - 1),
    (pos.fst + 1, pos.snd), (pos.fst, pos.snd + 1)]


namespace Std.Internal.Parsec 
open Std.Internal.Parsec
open Std.Internal.Parsec.String

def icoord: Parser ICoord := do
   let x <- int
   ws *> skipString ","
   let y <- int
   return ((x,y) : ICoord)
   
end Std.Internal.Parsec
