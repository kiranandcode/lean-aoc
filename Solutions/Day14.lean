import Batteries
import Utils

def input := AOC.getInput 14 (year:=.some 2024)

def testInput := "p=0,4 v=3,-3
p=6,3 v=-1,-3
p=10,3 v=-1,2
p=2,0 v=2,-1
p=0,0 v=1,3
p=3,0 v=-2,-2
p=7,6 v=-1,-3
p=3,0 v=-1,-2
p=9,3 v=2,3
p=7,3 v=-1,2
p=2,4 v=2,-3
p=9,5 v=-3,-3"

def parser :=
   open Std.Internal.Parsec in
   open Std.Internal.Parsec.String in do
   let p <- skipString "p=" *> icoord
   ws
   let v <- skipString "v=" *> icoord
   return (p,v)

def parseLine (s: String) := parser.run s |> Except.toOption

def move (w: Nat) (h: Nat) (steps: Nat) (r: ICoord × ICoord) :=
  r.fst.add (r.snd.mul steps)
  |>.mod w h

def countQuadrants (w: Nat) (h: Nat) (pos: List ICoord) :=
   let leftHalfV := (fun (p: ICoord) => p.x.toNat < w/2)
   let rightHalfV := (fun (p: ICoord) => p.x.toNat >= (w + 1)/2)
   let upperHalfH := (fun (p: ICoord) => p.y.toNat < h/2)
   let lowerHalfH := (fun (p: ICoord) => p.y.toNat >= (h + 1)/2)

   let nw := pos.countP (fun p => leftHalfV p && upperHalfH p)
   let sw := pos.countP (fun p => leftHalfV p && lowerHalfH p)
   let ne := pos.countP (fun p => rightHalfV p && upperHalfH p)
   let se := pos.countP (fun p => rightHalfV p && lowerHalfH p)

   ne * se * nw * sw

def parse (input: String) :=
   input
   |>.splitLines
   |>.filterMap parseLine

def process (w: Nat) (h: Nat) (input: String) :=
   parse input
   |>.map (move w h 100)
   |> countQuadrants w h

#example process 11 7 testInput evaluates to 12
#example process 101 103 <$> input evaluates to 230686500


def visualise (w: Nat) (h: Nat) (coords: List ICoord)  : String := Id.run $ do
   let mut arr := Array.mkArray h (Array.mkArray w ' ')
   for coord in coords do
      arr := arr.set2D! coord.swap.toCoord '*'
   return arr.map (fun row => s!"{String.mk row.toList}\n")
     |>.toList
     |> String.concat


def step (w: Nat) (h: Nat) (states: List (ICoord × ICoord)) :=
  states.map (fun p =>
   ((p.fst.add p.snd).mod w h, p.snd)
  )

def process' (input: String) := Id.run $ do
   let mut state <- parse input
   let mut currentStep := 0
   let mut seenStates := Std.HashSet.empty
   while !seenStates.contains state do
       currentStep := currentStep + 1
       let xAvg := (state.map (·.fst.fst)).sum / state.length
       let yAvg := (state.map (·.fst.snd)).sum / state.length
       let xVar := (state.map (fun p => (p.fst.fst - xAvg).pow 2)).sum / state.length
       let yVar := (state.map (fun p => (p.fst.snd - yAvg).pow 2)).sum / state.length 

       if xVar < 700 && yVar < 700 then
         break
       seenStates := seenStates.insert state
       state := step 101 103 state
   return (currentStep, visualise 101 103 (state.map (·.fst)))

def main : IO Unit := do
   let i <- input
   let (step, repr) := process' i
   println! "at step {step}"
   println! "{repr}"
   
