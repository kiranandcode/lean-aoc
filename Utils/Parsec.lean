import Batteries

namespace Std.Internal.Parsec
open Std.Internal.Parsec.String
open Std.Internal.Parsec

partial def allMatchesInt {V} (P: Parser V) (acc: Array V) : Parser (List V) := 
    (attempt P).tryCatch
      (allMatchesInt P <| acc.push ·)
      (fun () =>
         any.tryCatch
           (fun _ => allMatchesInt P acc)
           (fun () => pure acc.toList)) 

def allMatches {V} (P: Parser V) := allMatchesInt P #[]

end Std.Internal.Parsec
