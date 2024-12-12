import Batteries

def Std.HashSet.first? [Hashable A] [BEq A] (map: Std.HashSet A) : Option A := 
   match map.foldM (fun _ (k: A) => .error k) () with 
   | Except.ok () => none
   | Except.error e => e

def Std.HashSet.first! [Inhabited A] [Hashable A] [BEq A] (map: Std.HashSet A) : A :=
   map.first?.get!

