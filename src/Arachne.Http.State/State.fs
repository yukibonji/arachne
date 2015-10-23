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
open System.Net
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

type CookiePair =
    | CookiePair of CookieName * CookieValue

    static member internal Mapping =

        let cookieP =
                CookieName.Mapping.Parse 
            .>> skipChar '='
           .>>. CookieValue.Mapping.Parse
            |>> CookiePair

        let cookieF =
            function | CookiePair (n, v) ->
                            CookieName.Mapping.Format n 
                         >> append "=" 
                         >> CookieValue.Mapping.Format v

        { Parse = cookieP
          Format = cookieF }

    (* Lenses *)

    static member Name_ =
        (fun (CookiePair (n, _)) -> n), (fun n (CookiePair (_, v)) -> CookiePair (n, v))

    static member Value_ =
        (fun (CookiePair (_, v)) -> v), (fun v (CookiePair (n, _)) -> CookiePair (n, v))

    (* Common *)

    static member Format =
        Formatting.format CookiePair.Mapping.Format

    static member Parse =
        Parsing.parse CookiePair.Mapping.Parse

    static member TryParse =
        Parsing.tryParse CookiePair.Mapping.Parse

    override x.ToString () =
        CookiePair.Format x

and CookieName =
    | CookieName of string

    static member internal Mapping =

        let cookieNameP =
            tokenP |>> CookieName

        let cookieNameF =
            function | CookieName x -> append x

        { Parse = cookieNameP
          Format = cookieNameF }

    (* Lenses *)

    static member Name_ =
        (fun (CookieName n) -> n), (fun n -> CookieName n)

and CookieValue =
    | CookieValue of string

    static member internal Mapping =

        // TODO: Full value parser

        let cookieValueP =
            tokenP |>> CookieValue

        let cookieValueF =
            function | CookieValue x -> append x

        { Parse = cookieValueP
          Format = cookieValueF }

    (* Lenses *)

    static member Value_ =
        (fun (CookieValue v) -> v), (fun v -> CookieValue v)

(* Set-Cookie

   Taken from RFC 6265, Section 4.1 Set-Cookie
   See [http://tools.ietf.org/html/rfc6265#section-4.1] *)

type SetCookie =
    | SetCookie of CookiePair * CookieAttributes

    static member internal Mapping =

        let setCookieP =
                CookiePair.Mapping.Parse
           .>>. CookieAttributes.Mapping.Parse
            |>> fun (p, v) -> SetCookie (p, v)

        let setCookieF =
            function | SetCookie (p, a) ->
                            CookiePair.Mapping.Format p
                         >> CookieAttributes.Mapping.Format a

        { Parse = setCookieP
          Format = setCookieF }

    (* Lenses *)

    static member Cookie_ =
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

and CookieAttributes =
    | CookieAttributes of CookieAttribute list

    static member internal Mapping =

        let cookieAttributesP =
                many CookieAttribute.Mapping.Parse
            |>> CookieAttributes

        let cookieAttributesF =
            function | CookieAttributes a -> join CookieAttribute.Mapping.Format id a

        { Parse = cookieAttributesP
          Format = cookieAttributesF }

    (* Lenses *)

    static member Attributes_ =
        (fun (CookieAttributes a) -> a), (fun a -> CookieAttributes a)

and CookieAttribute =
    | Expires of DateTime
    | MaxAge of TimeSpan
    | Domain of Domain
    | Path of Path
    | Secure
    | HttpOnly

    static member internal Mapping =

        let expiresP =
            skipString "Expires=" >>. (httpDateP (manySatisfy ((<>) ';'))) |>> Expires

        let maxAgeP =
            skipString "Max-Age=" >>. puint32 |>> (float >> TimeSpan.FromSeconds >> MaxAge)

        let domainP =
            skipString "Domain=" >>. Domain.Mapping.Parse |>> Domain

        let pathP =
            skipString "Path=" >>. Path.Mapping.Parse |>> Path

        let secureP =
            skipString "Secure" >>% Secure

        let httpOnlyP =
            skipString "HttpOnly" >>% HttpOnly

        let cookieAttributeP =
                skipChar ';'
            >>. spP
            >>. choice [
                    expiresP
                    maxAgeP
                    domainP
                    pathP
                    secureP
                    httpOnlyP ]

        let cookieAttributeF =
            function | Expires x -> appendf1 "; Expires={0}" (x.ToString "r")
                     | MaxAge x -> appendf1 "; Max-Age={0}" (int x.TotalSeconds)
                     | Domain x -> appendf1 "; Domain={0}" (string x)
                     | Path x -> appendf1 "; Path={0}" (string x)
                     | Secure -> append "; Secure"
                     | HttpOnly -> append "; HttpOnly"

        { Parse = cookieAttributeP
          Format = cookieAttributeF }

and Domain =
    | IPv4 of IPAddress
    | IPv6 of IPAddress
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

and Path =
    | Path of string

    static member internal Mapping =

        // TODO: Full path parser

        let pathP =
            anyString 6 |>> Path

        let pathF =
            function | Path p -> append p

        { Parse = pathP
          Format = pathF }

    (* Lenses *)

    static member Path_ =
        (fun (Path p) -> p), (fun p -> Path p)

    (* Common *)

    static member Format =
        Formatting.format Path.Mapping.Format

    static member Parse =
        Parsing.parse Path.Mapping.Parse
    
    static member TryParse =
        Parsing.tryParse Path.Mapping.Parse

    override x.ToString () =
        Path.Format x

(* Cookie

   Taken from RFC 6265, Section 4.2 Cookie
   See [http://tools.ietf.org/html/rfc6265#section-4.2] *)

type Cookie =
    | Cookie of CookiePair

    static member internal Mapping =

        let cookieP =
                CookiePair.Mapping.Parse
            |>> Cookie

        let cookieF =
            function | Cookie c -> CookiePair.Mapping.Format c

        { Parse = cookieP
          Format = cookieF }

    (* Lenses *)

    static member Cookie_ =
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