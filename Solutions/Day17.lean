import Batteries
import Utils

def input := AOC.getInput 17

def testInput := "Register A: 729
Register B: 0
Register C: 0

Program: 0,1,5,4,3,0"
abbrev Integer := UInt64
def toInteger := UInt64.ofNat
structure State where
  registers : Array Integer
  pc: Nat
  output: List Nat
deriving Repr, Inhabited, BEq, Hashable
instance : ToString State where
  toString s := s!"PC: {s.pc},A,B,C: [{s.registers[0]!},{s.registers[1]!},{s.registers[2]!}]"


section Machine
variable {M: Type -> Type} [Monad M] [MonadStateOf State M]

def getRegisterA : M Integer := get |> Functor.map (·.registers[0]!)
def getRegisterB : M Integer := get |> Functor.map (·.registers[1]!)
def getRegisterC : M Integer := get |> Functor.map (·.registers[2]!)
def programCounter : M Nat := get |> Functor.map (·.pc)
def getOutput : M (List Nat) := get |> Functor.map (·.output.reverse)

def setRegisterA (v: Integer) : M Unit := do
   let s <- get
   set {s with registers := s.registers.set! 0 v}
def setRegisterB (v: Integer) : M Unit := do
   let s <- get
   set {s with registers := s.registers.set! 1 v}
def setRegisterC (v: Integer) : M Unit := do
   let s <- get
   set {s with registers := s.registers.set! 2 v}
def setProgramCounter (v: Nat) : M Unit := do
   let s <- get
   set {s with pc := v}
def incrPC : M Unit := do
   let s <- get
   set {s with pc := s.pc + 2}
def output (n: Nat) : M Unit :=  do
   let s <- get
   set {s with output := s.output.cons n}


def comboOperand : Nat -> M Integer
| 0 => return 0 | 1 => return 1 | 2 => return 2 | 3 => return 3
| 4 => getRegisterA
| 5 => getRegisterB
| 6 => getRegisterC
| n => return (toInteger n)

def comboOperandToString : Nat -> String
| 0 => "0" | 1 => "1" | 2 => "2" | 3 => "3"
| 4 => "A"
| 5 => "B"
| 6 => "C"
| _ => "?"


  
def step (cmd: Nat) (op: Nat) : M Unit := match cmd with
| 0 => do
-- adv
   let num <- getRegisterA
   let op <- comboOperand op
   let denom := 2 ^ op.toNat
   let res := num.toNat / denom
   setRegisterA (toInteger res)
   incrPC
| 1 => do
-- bxl
   let v1 <- getRegisterB
   let res := v1.xor (toInteger op)
   setRegisterB res
   incrPC
| 2 => do
-- bst
   let vl <- comboOperand op
   let res := vl % 8
   setRegisterB res
   incrPC
| 3 => do
-- jnz
   let vl <- getRegisterA
   if vl != 0 then
      setProgramCounter op
   else
      incrPC
| 4 => do
-- bxc
   let v1 <- getRegisterB
   let v2 <- getRegisterC
   let res := v1.xor v2
   setRegisterB res
   incrPC
| 5 => do
-- out
   let v1 <- comboOperand op
   let res := v1 % 8
   output res.toNat
   incrPC
| 6 => do
-- bdv
   let num <- getRegisterA
   let op <- comboOperand op
   let denom := 2 ^ op.toNat
   let res := num.toNat / denom
   setRegisterB (toInteger res)
   incrPC
| 7 => do
-- cdv
   let num <- getRegisterA
   let op <- comboOperand op
   let denom := 2 ^ op.toNat
   let res := num.toNat / denom
   setRegisterC (toInteger res)
   incrPC
| _ => return

def opToString (cmd: Nat) (op: Nat) : M String := match cmd with
| 0 => do
   let op' <- comboOperand op
   let a <- getRegisterA
   return s!"A <- A / 2^{comboOperandToString op}   [{a}/2^{op'}]"
| 1 => do
   let b <- getRegisterB
   return s!"B <- B ^^ {op}    [{b} ^^ {op}]"
| 2 => do
   let op' <- comboOperand op
   return s!"B <- {comboOperandToString op} % 8   [{op'} % 8]"
| 3 => do
   let vl <- getRegisterA
   return s!"if A != 0 then PC <- {op}       [{vl} != 0]"
| 4 => do
   let b <- getRegisterB
   let c <- getRegisterB
   return s!"B <- B ^ C     [{b} ^ {c}]"
| 5 => do
   let vl <- comboOperand op
   return s!"out <- {comboOperandToString op}      [{vl}]"
| 6 => do
   let op' <- comboOperand op
   let a <- getRegisterA
   return s!"B <- A / 2^{comboOperandToString op}   [{a}/2^{op'}]"
| 7 => do
   let op' <- comboOperand op
   let a <- getRegisterA
   return s!"C <- A / 2^{comboOperandToString op}   [{a}/2^{op'}]"
| _ => return s!""


def run' (s: State) (m: StateM State A) := StateT.run' m s

end Machine

def execute (print: Bool := false) (input: Array Nat × Array Integer) : IO (List Nat) := do
 let (cmds, initRegisters) := input
 flip StateT.run' ({registers := initRegisters, output := [], pc := 0 : State}) $ do
   let mut pc := (<- programCounter)
   while pc + 1 < cmds.size do
      let cmd := cmds[pc]!
      let op := cmds[pc + 1]!
      if print then
         println! "{pc}:({cmd},{op}): {(<-get)} [{<- opToString cmd op}]"
      step cmd op
      if print then
         println! "         {(<-get)}"
      pc <- programCounter
   return (<- getOutput)

def prettyOutput (ls: List Nat) := ls.map (fun n => s!"{n}") |> String.concat (sepBy := ",")

def parseInput (input: String) :=
  let args := input.splitOn "\n\n"
  let registers := args[0]!.splitLines
           |>.map (·.splitOn ":")
           |>.map (·[1]!.trim.toNat!)
           |>.map (toInteger ·)
           |>.toArray 
  let program := args[1]!.splitOn ":"
                 |>.get! 1
                 |>.splitOn ","
                 |>.map (·.trim.toNat!)
                 |>.toArray
  (program, registers)

def process (input: String) : IO String := do
  let input := parseInput input
  let output <- execute false input
  return prettyOutput output

#example process testInput evaluates to "4,6,3,5,6,3,5,2,1,0"
#example do process (<- input) evaluates to "6,0,6,3,0,2,3,1,6"

def update (a: Integer) :=
   let tmp1 := ((a % 8).xor 3)
   let c := a.toNat / 2 ^ tmp1.toNat |> toInteger
   ((tmp1.xor 5).xor c) % 8 |>.toNat

partial def run (a: Integer) :=
  let (a, b) := (a / 8, update a)
  if a != 0
  then b :: run a
  else [b]

-- first confirm that run, and our update function simulates the machine
#example run 55593699 evaluates to [6,0,6,3,0,2,3,1,6]

def reconstruct (vl: Nat) (a: Integer) :=
   List.range 8
   |>.map (fun v => (a + toInteger v))
   |>.filter (update · == vl)

def solve (program: List Nat) : List Integer :=
   program.reverse
   |>.foldl (fun (avls: List Integer) (targetvl: Nat) => 
     avls.flatMap (fun avl => 
        reconstruct targetvl (avl * 8) 
     )
   ) [0]
   
#example solve [6,0,6,3,0,2,3,1,6] |>.map run |>.all (· == [6,0,6,3,0,2,3,1,6]) evaluates to true
#example solve [2,4,1,3,7,5,0,3,1,5,4,4,5,5,3,0] |>.min?.get!
evaluates to (236539226447469: Integer)
