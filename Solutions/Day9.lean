import Batteries
import Utils

def input := AOC.getInput 9

def testInput := "2333133121414131402"

structure FileRecord where
  id: Nat
  start: Nat
  len: Nat
deriving Inhabited, BEq

def FileRecord.idChar (f: FileRecord): Char := (Char.toNat '0' + f.id |> Char.ofNat)

def FileRecord.checksum (f: FileRecord) := Id.run $ do
   let mut sum := 0
   for i in [f.start:f.start+f.len] do
      sum := sum + i * f.id
   return sum

instance : ToString FileRecord where
  toString f := s!"{String.mk (List.replicate f.len f.idChar)}@{f.start}"


-- src.moveTo dest ==> (newDest, oldsrc?, olddest?) --- returns a triple of the new destination, plus either
def FileRecord.moveTo (src: FileRecord) (destination: FileRecord) :=
   if destination.len < src.len
   then (FileRecord.mk src.id destination.start destination.len, some {src with len:=src.len - destination.len}, none)
   else if destination.len == src.len
   then (FileRecord.mk src.id destination.start src.len, none, none)
   else (FileRecord.mk src.id destination.start src.len, none, some {destination with start := destination.start + src.len, len := destination.len - src.len})

#example ((FileRecord.mk 2 10 5).moveTo (FileRecord.mk 0 1 2))
  evaluates to (
  ({ id := 2, start := 1, len := 2 } : FileRecord),
  some ({ id := 2, start := 10, len := 3 }: FileRecord),
  (none: Option FileRecord)
)



def visualise (ls: Array FileRecord) := Id.run $ do
   let sz := ls.map (fun fr => fr.start + fr.len) |>.max?.get?
   let mut arr := Array.mk (List.replicateTR sz '.')
   for elt in ls do
      for i in [elt.start:elt.start+elt.len] do
        arr := arr.set! i elt.idChar
   return String.mk arr.toList


partial def compact (used: Array FileRecord) (free: Array FileRecord) : Array FileRecord := Id.run $ do
   -- sort in reverse
   let mut used := used.toList.mergeSort (fun fr1 fr2 => fr2.start <= fr1.start)
   let mut free := free.qsort (fun fr1 fr2 => fr2.start <= fr1.start)

   while !free.isEmpty && !used.isEmpty do

      let freeSpace := free.back!
      free := free.pop

      let usedSpace := used.head!

      if freeSpace.start >= usedSpace.start then
         continue

      used := used.tail!

      let (newBlk, usedSpace, freeSpace) := usedSpace.moveTo freeSpace

      used := used.insertP (fun fr => newBlk.start > fr.start) newBlk

      if let some usedSpace := usedSpace then
         used := usedSpace :: used
      if let some freeSpace := freeSpace then
         free := free.push freeSpace

   used := used.mergeSort (fun fr1 fr2 => fr1.start <= fr2.start)
   return used.toArray


def process (input: String): Nat :=
  let data := input.data.map Char.toDigit
  let (used, free) :=
    flip data.foldl2D (0, 0, #[], #[]) (fun (id, pos, used, free) (bsz, fsz) =>
        let usedStart := pos
        let freeStart := usedStart + bsz
        let finalPos := freeStart + fsz
        let usedBlock := FileRecord.mk id usedStart bsz
        let freeBlock := FileRecord.mk 0 freeStart fsz
        (id + 1, finalPos, used.push usedBlock, if freeBlock.len > 0 then free.push freeBlock else free)
    )
    |>.snd.snd
  let used := compact used free
  used.map FileRecord.checksum |>.foldl (Nat.add) 0



#example process testInput evaluates to 1928
#example process <$> input evaluates to 6341711060162

partial def compactFiles (used: Array FileRecord) (free: Array FileRecord) : Array FileRecord := Id.run $ do
   -- sort in reverse
   let mut used := used.toList.mergeSort (fun fr1 fr2 => fr2.start <= fr1.start)
   let mut free := free.qsort (fun fr1 fr2 => fr2.start <= fr1.start)

   while !free.isEmpty && !used.isEmpty do

      let freeSpace := free.back!
      free := free.pop

      let usedSpace? := used.takeFind? (·.len <= freeSpace.len)
      if usedSpace?.isNone then
         continue
      let (usedSpace, usedTail) := usedSpace?.get!

      if freeSpace.start >= usedSpace.start then
         continue

      used := usedTail

      let (newBlk, usedSpace, freeSpace) := usedSpace.moveTo freeSpace

      used := used.insertP (fun fr => newBlk.start > fr.start) newBlk

      if let some usedSpace := usedSpace then
         used := usedSpace :: used
      if let some freeSpace := freeSpace then
         free := free.push freeSpace

   used := used.mergeSort (fun fr1 fr2 => fr1.start <= fr2.start)
   return used.toArray

def process' (input: String) :=
  let data := input.data.map Char.toDigit
  let (used, free) :=
    flip data.foldl2D (0, 0, #[], #[]) (fun (id, pos, used, free) (bsz, fsz) =>
        let usedStart := pos
        let freeStart := usedStart + bsz
        let finalPos := freeStart + fsz
        let usedBlock := FileRecord.mk id usedStart bsz
        let freeBlock := FileRecord.mk 0 freeStart fsz
        (id + 1, finalPos, used.push usedBlock, if freeBlock.len > 0 then free.push freeBlock else free)
    )
    |>.snd.snd
  let used := compactFiles used free
  used.map FileRecord.checksum |>.foldl (Nat.add) 0


#example process' testInput evaluates to 2858
#example process' <$> input evaluates to 6377400869326
  
