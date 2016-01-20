[<AutoOpen>]
module Arachne.Core.Tests.Prelude

open Swensen.Unquote.Assertions
open System.Globalization
open System.Threading

(* Helpers *)

let inline (=?) a b = test <@ a = b @>

let private culturesToTest = [
    CultureInfo("en")
    CultureInfo("de") ]

let private useCulture c =
    let current = Thread.CurrentThread.CurrentCulture
    Thread.CurrentThread.CurrentCulture <- c
    { new System.IDisposable with
          member x.Dispose() = Thread.CurrentThread.CurrentCulture <- current }

let roundTrip<'a when 'a: equality> (iso: ('a -> string) * (string -> 'a)) =
        List.collect (fun p -> List.map (fun x -> x, p) culturesToTest)
     >> List.iter (fun (c, (a, s)) ->
            use __ = useCulture c
            (fst iso) a =? s
            (snd iso) s =? a)
