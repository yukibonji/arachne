[<AutoOpen>]
module Arachne.Core.Tests.Prelude

open Swensen.Unquote.Assertions

(* Helpers *)

let inline (=?) a b = test <@ a = b @>

let roundTrip<'a when 'a: equality> (iso: ('a -> string) * (string -> 'a)) =
    List.iter (fun (a, s) ->
        (fst iso) a =? s
        (snd iso) s =? a)
