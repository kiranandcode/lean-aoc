import Batteries
import Utils

def input := AOC.getInput 13
def testInput := "Button A: X+94, Y+34
Button B: X+22, Y+67
Prize: X=8400, Y=5400

Button A: X+26, Y+66
Button B: X+67, Y+21
Prize: X=12748, Y=12176

Button A: X+17, Y+86
Button B: X+84, Y+37
Prize: X=7870, Y=6450

Button A: X+69, Y+23
Button B: X+27, Y+71
Prize: X=18641, Y=10279"

abbrev ICoord := (Int × Int)

def ICoord.x (c: ICoord) : Int := c.fst
def ICoord.y (c: ICoord) : Int := c.snd


structure Configuration where
   A: ICoord
   B: ICoord
   P: ICoord
deriving Inhabited, Repr, BEq, Hashable

section Parsing 
open Std.Internal.Parsec
open Std.Internal.Parsec.String

def int : Parser Int :=
     (skipChar '+' *> digits |> Functor.map Int.ofNat)
 <|> (skipChar '-' *> digits  |> Functor.map Int.ofNat |> Functor.map Int.neg)
 <|> (digits |> Functor.map Int.ofNat)



def parseButton : Parser (Int × Int) := do
   let _ <- skipString "Button" *> ws *> any
   ws *> skipChar ':' *> ws
   let x <- skipChar 'X' *> int
   ws *> skipChar ',' *> ws
   let y <- skipChar 'Y' *>int
   return (x,y)

def parsePrize : Parser (Int × Int) := do
   skipString "Prize:" *> ws
   let x <- skipString "X=" *> int
   ws *> skipChar ',' *> ws
   let y <- skipString "Y=" *> int
   return (x,y)

def parseConfig : Parser Configuration := do
   let a <- parseButton <* skipString "\n"
   let b <- parseButton  <* skipString "\n"
   let prize <- parsePrize
   return Configuration.mk a b prize

def parse s := parseConfig.run s |> Except.toOption

  
section Parsing

-- each configuration is equivalent to the following linear equation
-- [Ax Bx  [i    [Px
--  Ay By]  j] =  Py]
-- we can solve for i and j by inverting the matrix.
--          1           [ By  -Bx] [Px       [By * Px + -Bx * Py]
-- ------------------   [-Ay   Ax]  Py]  =   [-Ay * Px + Ax * Py]
--  Ax x By - Bx x Ay   

def Configuration.solve (c: Configuration) :=
  let {A, B, P} := c
  let det := A.x * B.y - (B.x * A.y)
  let x := B.y * P.x + (- B.x) * P.y
  let y := (-A.y) * P.x + A.x * P.y
  if x % det == 0 && y % det == 0
  then (x/det * 3 +  y/det * 1)
  else 0

  
  
def process (input: String) :=
  input.splitOn "\n\n"
  |>.filterMap parse
  |>.map Configuration.solve
  |>.sum


#example process testInput evaluates to (480: Int)
#example process <$> input evaluates to (36954: Int)

def process' (input: String) :=
  input.splitOn "\n\n"
  |>.filterMap parse
  |>.map (fun c => {c with P:=(c.P.x + 10000000000000, c.P.y + 10000000000000)})
  |>.map Configuration.solve
  |>.sum


#eval process' <$> input
