﻿//----------------------------------------------------------------------------
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

module Arachne.Http.State

open System
open Arachne.Core
open Arachne.Http
open Arachne.Uri
open FParsec

(* RFC 6265

   Types, parsers and formatters implemented to mirror the specification of
   HTTP State Management Mechanism semantics (commonly known as cookies) as
   defined in RFC 6265.

   Taken from [http://tools.ietf.org/html/rfc6265] *)

[<RequireQualifiedAccess>]
module Grammar =

    [<RequireQualifiedAccess>]
    module Parse =

        let sp =
            skipSatisfy (int >> Grammar.isSp)

(* Aliases *)

module F = Formatting
module G = Grammar
module M = Mapping

(* Cookie Common Types

   Cookie Pair, as defined for both Set-Cookie and Cookie headers, given
   in 4.1 and 4.2. *)

type Pair =
    | Pair of Name * Value

    static member Mapping =

        let pairP =
                Name.Mapping.Parse 
            .>> skipChar '='
           .>>. Value.Mapping.Parse
            |>> Pair

        let pairF =
            function | Pair (n, v) ->
                            Name.Mapping.Format n 
                         >> F.append "=" 
                         >> Value.Mapping.Format v

        { Parse = pairP
          Format = pairF }

    (* Optics *)

    static member name_ =
        (fun (Pair (n, _)) -> n), (fun n (Pair (_, v)) -> Pair (n, v))

    static member value_ =
        (fun (Pair (_, v)) -> v), (fun v (Pair (n, _)) -> Pair (n, v))

    (* Common *)

    static member format =
        M.format Pair.Mapping

    static member parse =
        M.parse Pair.Mapping

    static member tryParse =
        M.tryParse Pair.Mapping

    override x.ToString () =
        Pair.format x

    (* Obsolete

       To be removed in 4.0. *)

    [<Obsolete ("Use name_ instead.")>]
    static member Name_ =
        Pair.name_

    [<Obsolete ("Use value_ instead.")>]
    static member Value_ =
        Pair.value_

    [<Obsolete ("Use format instead.")>]
    static member Format =
        Pair.format

    [<Obsolete ("Use parse instead.")>]
    static member Parse =
        Pair.parse

    [<Obsolete ("Use tryParse instead.")>]
    static member TryParse =
        Pair.tryParse

and Name =
    | Name of string

    static member Mapping =

        let nameP =
            G.Parse.token |>> Name

        let nameF =
            function | Name x -> F.append x

        { Parse = nameP
          Format = nameF }

    (* Optics *)

    static member name_ =
        (fun (Name n) -> n), (Name)

    (* Obsolete

       To be removed in 4.0. *)

    [<Obsolete ("Use name_ instead.")>]
    static member Name_ =
        Name.name_

and Value =
    | Value of string

    static member Mapping =

        let isCookieOctet i =
                i = 0x21 // !
             || i >= 0x23 && i <= 0x2b
             || i >= 0x2d && i <= 0x3a
             || i >= 0x3c && i <= 0x5b
             || i >= 0x5d && i <= 0x7e

        let cookieOctetsP =
            manySatisfy (int >> isCookieOctet)

        let valueP =
            skipChar '"' >>. cookieOctetsP .>> skipChar '"' <|> cookieOctetsP |>> Value

        let valueF =
            function | Value x -> F.append x

        { Parse = valueP
          Format = valueF }

    (* Optics *)

    static member value_ =
        (fun (Value v) -> v), (Value)

    (* Obsolete

       To be removed in 4.0. *)

    [<Obsolete ("Use value_ instead.")>]
    static member Value_ =
        Value.value_

(* Set-Cookie

   Taken from RFC 6265, Section 4.1 Set-Cookie
   See [http://tools.ietf.org/html/rfc6265#section-4.1] *)

type SetCookie =
    | SetCookie of Pair * Attributes

    static member Mapping =

        let setCookieP =
                Pair.Mapping.Parse
           .>>. Attributes.Mapping.Parse
            |>> fun (p, v) -> SetCookie (p, v)

        let setCookieF =
            function | SetCookie (p, a) ->
                            Pair.Mapping.Format p
                         >> Attributes.Mapping.Format a

        { Parse = setCookieP
          Format = setCookieF }

    (* Optics *)

    static member pair_ =
        (fun (SetCookie (p, _)) -> p), (fun p (SetCookie (_, a)) -> SetCookie (p, a))

    static member attributes_ =
        (fun (SetCookie (_, a)) -> a), (fun a (SetCookie (p, _)) -> SetCookie (p, a))

    (* Common *)

    static member format =
        M.format SetCookie.Mapping

    static member parse =
        M.parse SetCookie.Mapping

    static member tryParse =
        M.tryParse SetCookie.Mapping

    override x.ToString () =
        SetCookie.format x

    (* Obsolete

       To be removed in 4.0. *)

    [<Obsolete ("Use pair_ instead.")>]
    static member Pair_ =
        SetCookie.pair_

    [<Obsolete ("Use attributes_ instead.")>]
    static member Attributes_ =
        SetCookie.attributes_

    [<Obsolete ("Use format instead.")>]
    static member Format =
        SetCookie.format

    [<Obsolete ("Use parse instead.")>]
    static member Parse =
        SetCookie.parse

    [<Obsolete ("Use tryParse instead.")>]
    static member TryParse =
        SetCookie.tryParse

and Attributes =
    | Attributes of Attribute list

    static member Mapping =

        let attributesP =
                many Attribute.Mapping.Parse
            |>> Attributes

        let attributesF =
            function | Attributes a -> F.join Attribute.Mapping.Format id a

        { Parse = attributesP
          Format = attributesF }

    (* Optics *)

    static member attributes_ =
        (fun (Attributes a) -> a), (Attributes)

    (* Obsolete

       To be removed in 4.0. *)

    [<Obsolete ("Use attributes_ instead.")>]
    static member Attributes_ =
        Attributes.attributes_

and Attribute =
    | Expires of DateTime
    | MaxAge of TimeSpan
    | Domain of Domain
    | Path of string
    | Secure
    | HttpOnly

    static member Mapping =

        let isNonCtlSemiOctet i =
                i >= 0x20 && i <= 0x3a
             || i >= 0x3c && i <= 0x7e

        let nonCtlSemiOctetsP =
            manySatisfy (int >> isNonCtlSemiOctet)

        let expiresP =
            skipString "Expires=" >>. (HttpDate.Parse.httpDate (manySatisfy ((<>) ';'))) |>> Expires

        let maxAgeP =
            skipString "Max-Age=" >>. puint32 |>> (float >> TimeSpan.FromSeconds >> MaxAge)

        let domainP =
            skipString "Domain=" >>. Domain.Mapping.Parse |>> Domain

        let pathP =
            skipString "Path=" >>. nonCtlSemiOctetsP |>> Path

        let secureP =
            skipString "Secure" >>% Secure

        let httpOnlyP =
            skipString "HttpOnly" >>% HttpOnly

        let attributeP =
                skipChar ';'
            >>. G.Parse.sp
            >>. choice [
                    expiresP
                    maxAgeP
                    domainP
                    pathP
                    secureP
                    httpOnlyP ]

        let attributeF =
            function | Expires x -> F.appendf1 "; Expires={0}" (x.ToString "r")
                     | MaxAge x -> F.appendf1 "; Max-Age={0}" (int x.TotalSeconds)
                     | Domain x -> F.appendf1 "; Domain={0}" (string x)
                     | Path x -> F.appendf1 "; Path={0}" (string x)
                     | Secure -> F.append "; Secure"
                     | HttpOnly -> F.append "; HttpOnly"

        { Parse = attributeP
          Format = attributeF }

and Domain =
    | IPv4 of string
    | IPv6 of string
    | SubDomain of string

    static member Mapping =

        (* RFC 1034/1123

           Domain and Subdomain syntax is taken from RFC 1034 and updated by RFC 1123 (allowing
           the Domain to be an IP Address, and loosening the constraints on subdomains to
           allow the initial character to be numeric.

           This implementation is as simplistic as possible while still remaining consistent.
           Refactoring/reimplementation is welcomed. *)

        let isLetDig i =
                G.isAlpha i
             || G.isDigit i

        let isLetDigHyp i =
                isLetDig i
             || i = 0x2d // -

        let letDigP =
            satisfy (int >> isLetDig)

        let letDigHypP =
            satisfy (int >> isLetDigHyp)

        let endP =
            next2CharsSatisfyNot (fun _ c -> isLetDig (int c))

        let labelP =
                letDigP .>>. opt (manyCharsTill letDigHypP endP .>>. letDigP)
            |>> function | a, Some (b, c) -> string a + b + string c
                         | a, _ -> string a

        let subDomainP =
                sepBy1 labelP (skipChar '.')
            |>> (fun x -> SubDomain (String.Join (".", x)))

        let domainP =
            choice [
                IPAddress.Parse.v4 |>> IPv4
                IPAddress.Parse.v6 |>> IPv6
                subDomainP ]

        let domainF =
            function | IPv4 x -> IPAddress.Format.v4 x
                     | IPv6 x -> IPAddress.Format.v6 x
                     | SubDomain x -> F.append x

        { Parse = domainP
          Format = domainF }

    (* Optics *)

    static member ipv4_ =
        (function | IPv4 i -> Some i | _ -> None), (IPv4)

    static member ipv6_ =
        (function | IPv6 i -> Some i | _ -> None), (IPv6)

    static member subDomain_ =
        (function | SubDomain s -> Some s | _ -> None), (SubDomain)

    (* Common *)

    static member format =
        M.format Domain.Mapping

    static member parse =
        M.parse Domain.Mapping
    
    static member tryParse =
        M.tryParse Domain.Mapping

    override x.ToString () =
        Domain.format x

    (* Obsolete

       To be removed in 4.0. *)

    [<Obsolete ("Use ipv4_ instead.")>]
    static member IPv4_ =
        Domain.ipv4_

    [<Obsolete ("Use ipv6_ instead.")>]
    static member IPv6_ =
        Domain.ipv6_

    [<Obsolete ("Use subDomain_ instead.")>]
    static member SubDomain_ =
        Domain.subDomain_

    [<Obsolete ("Use format instead.")>]
    static member Format =
        Domain.format

    [<Obsolete ("Use parse instead.")>]
    static member Parse =
        Domain.parse

    [<Obsolete ("Use tryParse instead.")>]
    static member TryParse =
        Domain.tryParse

(* Cookie

   Taken from RFC 6265, Section 4.2 Cookie
   See [http://tools.ietf.org/html/rfc6265#section-4.2] *)

type Cookie =
    | Cookie of Pair list

    static member Mapping =

        let cookieP =
            sepBy1 Pair.Mapping.Parse (skipString "; ") |>> Cookie

        let cookieF =
            function | Cookie pairs -> F.join Pair.Mapping.Format (F.append "; ") pairs

        { Parse = cookieP
          Format = cookieF }

    (* Optics *)

    static member pairs_ =
        (fun (Cookie c) -> c), (Cookie)

    (* Common *)

    static member format =
        M.format Cookie.Mapping

    static member parse =
        M.parse Cookie.Mapping

    static member tryParse =
        M.tryParse Cookie.Mapping

    override x.ToString () =
        Cookie.format x

    (* Obsolete

       To be removed in 4.0. *)

    [<Obsolete ("Use pairs_ instead.")>]
    static member Pairs_ =
        Cookie.pairs_

    [<Obsolete ("Use format instead.")>]
    static member Format =
        Cookie.format

    [<Obsolete ("Use parse instead.")>]
    static member Parse =
        Cookie.parse

    [<Obsolete ("Use tryParse instead.")>]
    static member TryParse =
        Cookie.tryParse