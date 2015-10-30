//----------------------------------------------------------------------------
//
// Copyright (c) 2014
//
//    Ryan Riley (@panesofglass) and Andrew Cherry (@kolektiv)
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//    http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
//
//----------------------------------------------------------------------------

namespace Arachne.Core

open System.Runtime.CompilerServices
open System.Text
open FParsec

(* Internals *)

[<assembly:InternalsVisibleTo ("Arachne.Http")>]
[<assembly:InternalsVisibleTo ("Arachne.Http.Cors")>]
[<assembly:InternalsVisibleTo ("Arachne.Http.State")>]
[<assembly:InternalsVisibleTo ("Arachne.Language")>]
[<assembly:InternalsVisibleTo ("Arachne.Uri")>]
[<assembly:InternalsVisibleTo ("Arachne.Uri.Template")>]
do ()

[<AutoOpen>]
module internal Mapping =

    (* Types *)

    type Mapping<'a> =
        { Parse: Parse<'a>
          Format: Format<'a> }

    and Parse<'a> =
        Parser<'a,unit>

    and Format<'a> =
        'a -> StringBuilder -> StringBuilder

[<AutoOpen>]
module internal Formatting =

    (* Types *)

    let format (format: Format<'a>) =
        fun a -> string (format a (StringBuilder ()))

    (* Helpers *)

    type Separator =
        StringBuilder -> StringBuilder

    let append (s: string) (b: StringBuilder) =
        b.Append s

    let appendf1 (s: string) (v1: obj) (b: StringBuilder) =
        b.AppendFormat (s, v1)

    let appendf2 (s: string) (v1: obj) (v2: obj) (b: StringBuilder) =
        b.AppendFormat (s, v1, v2)

    let join<'a> (f: Format<'a>) (s: Separator) =
        let rec join values (b: StringBuilder) =
            match values with
            | [] -> b
            | [v] -> f v b
            | h :: t -> (f h >> s >> join t) b

        join

[<AutoOpen>]
module internal Parsing =

    (* Parsing *)

    let parse (p: Parse<'a>) s =
        match run p s with
        | Success (x, _, _) -> x
        | Failure (e, _, _) -> failwith e

    let tryParse (p: Parse<'a>) s =
        match run p s with
        | Success (x, _, _) -> Some x
        | Failure (_) -> None

[<AutoOpen>]
module internal Grammar =

    (* RFC 5234

       Core ABNF grammar rules as defined in RFC 5234, expressed
       as predicates over integer character codes.

       Taken from RFC 5234, Appendix B.1 Core Rules
       See [http://tools.ietf.org/html/rfc5234#appendix-B.1] *)

    let isAlpha i =
            i >= 0x41 && i <= 0x5a
         || i >= 0x61 && i <= 0x7a

    let isDigit i =
            i >= 0x30 && i <= 0x39

    let isDquote i =
            i = 0x22

    let isHexdig i =
            isDigit i
         || i >= 0x41 && i <= 0x46
         || i >= 0x61 && i <= 0x66

    let isHtab i =
            i = 0x09

    let isSp i =
            i = 0x20

    let isVchar i =
            i >= 0x21 && i <= 0x7e

    let isWsp i =
            isHtab i
         || isSp i