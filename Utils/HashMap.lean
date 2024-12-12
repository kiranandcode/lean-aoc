import Batteries
import Utils.Option
import Utils.Macros

namespace Std.HashMap
  variable {α : Type u} {β : Type v} {_ : BEq α} {_ : Hashable α}

  def update (h: HashMap α β) (k: α) (f: Option β -> β) : HashMap α β :=
      h.alter k (fun v => some (f v))
  
  def getD? [Inhabited β] (m : HashMap α β) (a : α) : β := m.getD a default

  partial def transitiveClosure {K} [BEq K] [Hashable K] (m: Std.HashMap K (List K)) : Std.HashMap K (List K) := Id.run $ do
     let mut anyChanges := true
     let mut m := m.map  (fun _ v => Std.HashSet.ofList v)
     while anyChanges do
        anyChanges := false
        for k in m.keys do
          let mut kset := m[k]!
          let sz := kset.size
          for dep in m[k]! do
             kset := kset.union m[dep]?.get?
          if sz != kset.size then
             anyChanges := true
             m := m.insert k kset
     return m.map (fun _ v => v.toList)

end Std.HashMap

#example Std.HashMap.ofList [('a',['b', 'c']), ('c',['d', 'e']), ('e',['f'])]
         |>.transitiveClosure
         |>.toList
         evaluates to  [('e', ['f']), ('c', ['f', 'e', 'd']), ('a', ['f', 'e', 'd', 'c', 'b'])]

def Std.HashMap.first? [Hashable A] [BEq A] (map: Std.HashMap A B) : Option A := 
   match map.foldM (fun _ (k: A) _ => .error k) () with 
   | Except.ok () => none
   | Except.error e => e

def Std.HashMap.first! [Inhabited A] [Hashable A] [BEq A] (map: Std.HashMap A B) : A :=
   map.first?.get!
