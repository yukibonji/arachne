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

module Arachne.Language

open Arachne.Core
open FParsec

(* RFC 5646

   Types, parsers and formatters implemented to mirror the specification of 
   Language Tag semantics as defined in RFC 5646.

   Taken from [http://tools.ietf.org/html/rfc5646] *)

(* Note: The current implementation does not implement either private use
   tags or grandfathered tags. Contributions are welcome if this is a real
   world issue for anyone.

   In addition, the main implementation of a language tag does not implement
   the "extension" property, or the optional "privateuse" property. As the RFC,
   even in the list of example language tags, never produces an example of either
   of these in use they have been assumed to be of low importance for now.

   However, if someone does show a valid and even slightly common use case,
   they will be implemented. *)

[<RequireQualifiedAccess>]
module Grammar =

    let isAlphaDigit i =
            Grammar.isAlpha i
         || Grammar.isDigit i

    [<RequireQualifiedAccess>]
    module internal Parse =

        let alpha min max =
                manyMinMaxSatisfy min max (int >> Grammar.isAlpha) 
           .>>? notFollowedBy (skipSatisfy (int >> Grammar.isAlpha))

        let digit min max =
                manyMinMaxSatisfy min max (int >> Grammar.isAlpha) 
           .>>? notFollowedBy (skipSatisfy (int >> Grammar.isAlpha))

        let alphaNum min max =
                manyMinMaxSatisfy min max (int >> isAlphaDigit) 
           .>>? notFollowedBy (skipSatisfy (int >> isAlphaDigit))

(* Aliases *)

module F = Formatting
module G = Grammar
module M = Mapping

(* Language *)

type Language =
    | Language of string * string list option

    static member Mapping =

        let extP =
            skipChar '-' >>. G.Parse.alpha 3 3

        let extLangP =
            choice [
                attempt (tuple3 extP extP extP) |>> fun (a, b, c) -> a :: b :: [ c ]
                attempt (tuple2 extP extP) |>> fun (a, b) -> a :: [ b ]
                extP |>> fun a -> [ a ] ]

        let languageP =
            choice [
                G.Parse.alpha 2 3 .>>. opt (attempt extLangP) |>> Language
                G.Parse.alpha 4 4 |>> (fun x -> Language (x, None))
                G.Parse.alpha 5 8 |>> (fun x -> Language (x, None)) ]

        let extF =
            function | x -> F.append "-" >> F.append x

        let extLangF =
            function | xs -> F.join extF id xs

        let languageF =
            function | Language (x, Some e) -> F.append x >> extLangF e
                     | Language (x, _) -> F.append x 

        { Parse = languageP
          Format = languageF }

    static member format =
        M.format Language.Mapping

    static member parse =
        M.parse Language.Mapping

    static member tryParse =
        M.tryParse Language.Mapping

    override x.ToString () =
        Language.format x

(* Language Tag *)

type LanguageTag =
    | LanguageTag of Language * Script option * Region option * Variant

    static member Mapping =

        let languageTagP =
            tuple4 Language.Mapping.Parse 
                   (opt (attempt Script.Mapping.Parse))
                   (opt (attempt Region.Mapping.Parse))
                   (Variant.Mapping.Parse)
            |>> fun (language, script, region, variant) ->
                LanguageTag (language, script, region, variant)

        let languageTagF =
            function | LanguageTag (language, script, region, variant) ->
                         let formatters =
                            [ Language.Mapping.Format language
                              (function | Some x -> Script.Mapping.Format x | _ -> id) script
                              (function | Some x -> Region.Mapping.Format x | _ -> id) region
                              Variant.Mapping.Format variant ]

                         fun b -> List.fold (|>) b formatters

        { Parse = languageTagP
          Format = languageTagF }

    static member format =
        M.format LanguageTag.Mapping

    static member parse =
        M.parse LanguageTag.Mapping

    static member tryParse =
        M.tryParse LanguageTag.Mapping

    override x.ToString () =
        LanguageTag.format x

(* Script *)

 and Script =
    | Script of string

    static member Mapping =

        let scriptP =
            skipChar '-' >>. G.Parse.alpha 4 4 |>> Script

        let scriptF =
            function | Script x -> F.append "-" >> F.append x

        { Parse = scriptP
          Format = scriptF }

(* Region *)

 and Region =
    | Region of string

    static member Mapping =

        let regionP =
            skipChar '-' >>. (G.Parse.alpha 2 2 <|> G.Parse.digit 3 3) |>> Region

        let regionF =
            function | Region x -> F.append "-" >> F.append x

        { Parse = regionP
          Format = regionF }

(* Variant *)

 and Variant =
    | Variant of string list

    static member Mapping =

        let alphaPrefixVariantP =
            G.Parse.alphaNum 5 8

        let digitPrefixVariantP =
            satisfy isDigit .>>. G.Parse.alphaNum 3 3 |>> fun (c, s) -> sprintf "%c%s" c s

        let varP =
            skipChar '-' >>. (alphaPrefixVariantP <|> digitPrefixVariantP)

        let variantP =
            many varP |>> Variant

        let varF =
            function | x -> F.append "-" >> F.append x

        let variantF =
            function | Variant xs -> F.join varF id xs

        { Parse = variantP
          Format = variantF }

(* RFC 4647

   Types, parsers and formatters implemented to mirror the specification of 
   Language Range semantics as defined in RFC 4647.

   Taken from [http://tools.ietf.org/html/rfc4647] *)

type LanguageRange =
    | Range of string list
    | Any

    static member Mapping =

        let languageRangeP =
            choice [
                skipChar '*' >>% Any
                G.Parse.alpha 1 8 .>>. many (skipChar '-' >>. G.Parse.alphaNum 1 8) |>> (fun (x, xs) -> Range (x :: xs)) ]


        let languageRangeF =
            function | Range x -> F.join F.append (F.append "-") x
                     | Any -> F.append "*"

        { Parse = languageRangeP
          Format = languageRangeF }

    static member format =
        M.format LanguageRange.Mapping

    static member parse =
        M.parse LanguageRange.Mapping

    static member tryParse =
        M.tryParse LanguageRange.Mapping

    override x.ToString () =
        LanguageRange.format x
