import Batteries
import Utils


def input : IO String := AOC.getInput 3

def exampleInput := "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"

section Parsing
open Std.Internal.Parsec.String
open Std.Internal.Parsec

def digits1to3 : Parser Nat := do
   let digits <- many1 digit
   if digits.size <= 3
   then pure <| digits.foldl (· * 10 + ·.toDigit) 0
   else fail "expected 1-3 digits"

#example digits1to3.run "123" |>.toOption evaluates to (some 123)
#example digits1to3.run "1" |>.toOption evaluates to (some 1)
#example digits1to3.run "1234" |>.toOption evaluates to (none: Option Nat)

def mulOp: Parser (Nat × Nat) := do
  skipString "mul("
  let n1 <- digits1to3
  skipChar ','
  let n2 <- digits1to3
  skipChar ')'
  return (n1, n2)

def mulOpCorruptedSequence : Parser (List (Nat × Nat)) :=
  allMatches mulOp 

def parse s := (Parser.run mulOpCorruptedSequence s).toOption.get!

#example parse "mul" evaluates to ([]: List (Nat × Nat))

#example parse "mul(1,2)" evaluates to [(1,2)]

#example parse "xmul(2,4)%mul(" evaluates to [(2,4)]

end Parsing


def process (i: String) := parse i |>.map (Function.uncurry Nat.mul) |>.sum

#example process <$> input evaluates to 175015740


section Parsing
open Std.Internal.Parsec.String
open Std.Internal.Parsec

def doOp : Parser Bool :=
   (skipString "do()" *> pure true) <|> (skipString "don't()" *> pure false)

def mulOrDoOp : Parser (Either Bool (Nat × Nat)) :=
   .right <$> mulOp <|> .left <$> doOp

def corruptedMulOrDoOp := allMatches mulOrDoOp

def parse' s := corruptedMulOrDoOp.run s 

end Parsing

def process' (s: String) :=
  parse' s
  |>.toOption
  |>.get?
  |>.map (Either.mapRight (Function.uncurry Nat.mul))
  |>.foldl (fun (isEnabled, sum) v =>
    match v with
    | .left v => (v, sum)
    | .right v =>
       if isEnabled
       then (isEnabled, sum + v)
       else (isEnabled, sum)
  ) (true, (0: Int))
  |>.snd


#example
  process' exampleInput
  evaluates to (48: Int)


#example process' <$> input evaluates to (112272912: Int)
