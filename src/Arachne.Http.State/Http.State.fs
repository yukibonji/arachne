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

namespace Arachne.Http.State

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

[<AutoOpen>]
module internal Grammar =

    let spP =
        skipSatisfy (int >> isSp)

(* Cookie Common Types

   Cookie Pair, as defined for both Set-Cookie and Cookie headers, given
   in 4.1 and 4.2. *)

type Pair =
    | Pair of Name * Value

    static member internal Mapping =

        let pairP =
                Name.Mapping.Parse 
            .>> skipChar '='
           .>>. Value.Mapping.Parse
            |>> Pair

        let pairF =
            function | Pair (n, v) ->
                            Name.Mapping.Format n 
                         >> append "=" 
                         >> Value.Mapping.Format v

        { Parse = pairP
          Format = pairF }

    (* Lenses *)

    static member Name_ =
        (fun (Pair (n, _)) -> n), (fun n (Pair (_, v)) -> Pair (n, v))

    static member Value_ =
        (fun (Pair (_, v)) -> v), (fun v (Pair (n, _)) -> Pair (n, v))

    (* Common *)

    static member Format =
        Formatting.format Pair.Mapping.Format

    static member Parse =
        Parsing.parse Pair.Mapping.Parse

    static member TryParse =
        Parsing.tryParse Pair.Mapping.Parse

    override x.ToString () =
        Pair.Format x

and Name =
    | Name of string

    static member internal Mapping =

        let nameP =
            tokenP |>> Name

        let nameF =
            function | Name x -> append x

        { Parse = nameP
          Format = nameF }

    (* Lenses *)

    static member Name_ =
        (fun (Name n) -> n), (fun n -> Name n)

and Value =
    | Value of string

    static member internal Mapping =

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
            function | Value x -> append x

        { Parse = valueP
          Format = valueF }

    (* Lenses *)

    static member Value_ =
        (fun (Value v) -> v), (fun v -> Value v)

(* Set-Cookie

   Taken from RFC 6265, Section 4.1 Set-Cookie
   See [http://tools.ietf.org/html/rfc6265#section-4.1] *)

type SetCookie =
    | SetCookie of Pair * Attributes

    static member internal Mapping =

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

    (* Lenses *)

    static member Pair_ =
        (fun (SetCookie (p, _)) -> p), (fun p (SetCookie (_, a)) -> SetCookie (p, a))

    static member Attributes_ =
        (fun (SetCookie (_, a)) -> a), (fun a (SetCookie (p, _)) -> SetCookie (p, a))

    (* Common *)

    static member Format =
        Formatting.format SetCookie.Mapping.Format

    static member Parse =
        Parsing.parse SetCookie.Mapping.Parse

    static member TryParse =
        Parsing.tryParse SetCookie.Mapping.Parse

    override x.ToString () =
        SetCookie.Format x

and Attributes =
    | Attributes of Attribute list

    static member internal Mapping =

        let attributesP =
                many Attribute.Mapping.Parse
            |>> Attributes

        let attributesF =
            function | Attributes a -> join Attribute.Mapping.Format id a

        { Parse = attributesP
          Format = attributesF }

    (* Lenses *)

    static member Attributes_ =
        (fun (Attributes a) -> a), (fun a -> Attributes a)

and Attribute =
    | Expires of DateTime
    | MaxAge of TimeSpan
    | Domain of Domain
    | Path of string
    | Secure
    | HttpOnly

    static member internal Mapping =

        let isNonCtlSemiOctet i =
                i >= 0x20 && i <= 0x3a
             || i >= 0x3c && i <= 0x7e

        let nonCtlSemiOctetsP =
            manySatisfy (int >> isNonCtlSemiOctet)

        let expiresP =
            skipString "Expires=" >>. (httpDateP (manySatisfy ((<>) ';'))) |>> Expires

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
            >>. spP
            >>. choice [
                    expiresP
                    maxAgeP
                    domainP
                    pathP
                    secureP
                    httpOnlyP ]

        let attributeF =
            function | Expires x -> appendf1 "; Expires={0}" (x.ToString "r")
                     | MaxAge x -> appendf1 "; Max-Age={0}" (int x.TotalSeconds)
                     | Domain x -> appendf1 "; Domain={0}" (string x)
                     | Path x -> appendf1 "; Path={0}" (string x)
                     | Secure -> append "; Secure"
                     | HttpOnly -> append "; HttpOnly"

        { Parse = attributeP
          Format = attributeF }

and Domain =
    | IPv4 of string
    | IPv6 of string
    | SubDomain of string

    static member internal Mapping =

        (* RFC 1034/1123

           Domain and Subdomain syntax is taken from RFC 1034 and updated by RFC 1123 (allowing
           the Domain to be an IP Address, and loosening the constraints on subdomains to
           allow the initial character to be numeric.

           This implementation is as simplistic as possible while still remaining consistent.
           Refactoring/reimplementation is welcomed. *)

        let isLetDig i =
                isAlpha i
             || Grammar.isDigit i

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
                ipv6AddressP |>> IPv6
                ipv4AddressP |>> IPv4
                subDomainP ]

        let domainF =
            function | IPv4 x -> ipv4AddressF x
                     | IPv6 x -> ipv6AddressF x
                     | SubDomain x -> append x

        { Parse = domainP
          Format = domainF }

    (* Lenses *)

    static member IPv4_ =
        (function | IPv4 i -> Some i | _ -> None), (fun i -> IPv4 i)

    static member IPv6_ =
        (function | IPv6 i -> Some i | _ -> None), (fun i -> IPv6 i)

    static member SubDomain_ =
        (function | SubDomain s -> Some s | _ -> None), (fun s -> SubDomain s)

    (* Common *)

    static member Format =
        Formatting.format Domain.Mapping.Format

    static member Parse =
        Parsing.parse Domain.Mapping.Parse
    
    static member TryParse =
        Parsing.tryParse Domain.Mapping.Parse

    override x.ToString () =
        Domain.Format x

(* Cookie

   Taken from RFC 6265, Section 4.2 Cookie
   See [http://tools.ietf.org/html/rfc6265#section-4.2] *)

type Cookie =
    | Cookie of Pair list

    static member internal Mapping =

        let cookieP =
            sepBy1 Pair.Mapping.Parse (skipString "; ") |>> Cookie

        let cookieF =
            function | Cookie pairs -> join Pair.Mapping.Format (append "; ") pairs

        { Parse = cookieP
          Format = cookieF }

    (* Lenses *)

    static member Pairs_ =
        (fun (Cookie c) -> c), (fun c -> Cookie c)

    (* Common *)

    static member Format =
        Formatting.format Cookie.Mapping.Format

    static member Parse =
        Parsing.parse Cookie.Mapping.Parse

    static member TryParse =
        Parsing.tryParse Cookie.Mapping.Parse

    override x.ToString () =
        Cookie.Format x