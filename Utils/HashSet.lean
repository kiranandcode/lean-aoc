import Batteries

def Std.HashSet.first? [Hashable A] [BEq A] (map: Std.HashSet A) : Option A := 
   match map.foldM (fun _ (k: A) => .error k) () with 
   | Except.ok () => none
   | Except.error e => e

def Std.HashSet.first! [Inhabited A] [Hashable A] [BEq A] (map: Std.HashSet A) : A :=
   map.first?.get!

def Std.HashSet.intersect [BEq A] [Hashable A] (s1 s2: Std.HashSet A) : Std.HashSet A :=
  s1.filter (s2.contains ·)

def Std.HashSet.intersectMany [BEq A] [Hashable A] (hshs: List (Std.HashSet A)) : Std.HashSet A :=
  match hshs with
  | [] => .empty
  | h :: t =>
     t.foldl Std.HashSet.intersect h
