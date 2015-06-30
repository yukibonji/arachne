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

type CookieName =
    | CookieName of string

    static member internal Mapping =

        let cookieNameP =
            tokenP |>> CookieName

        let cookieNameF =
            function | CookieName x -> append x

        { Parse = cookieNameP
          Format = cookieNameF }

and CookieValue =
    | CookieValue of string

    static member internal Mapping =

        (* TODO: Correct parsing grammar for cookie values. *)

        let cookieValueP =
            tokenP |>> CookieValue

        let cookieValueF =
            function | CookieValue x -> append x

        { Parse = cookieValueP
          Format = cookieValueF }

(* Set-Cookie

   Taken from RFC 6265, Section 4.1 Set-Cookie
   See [http://tools.ietf.org/html/rfc6265#section-4.1] *)

type SetCookie =
    | SetCookie of CookieName * CookieValue * CookieAttributes

    static member internal Mapping =

        let setCookieP =
                CookieName.Mapping.Parse 
            .>> skipChar '=' 
           .>>. CookieValue.Mapping.Parse 
           .>>. CookieAttributes.Mapping.Parse
            |>> fun ((n, a), v) -> SetCookie (n, a, v)

        let setCookieF =
            function | SetCookie (n, v, a) ->
                            CookieName.Mapping.Format n 
                         >> append "="
                         >> CookieValue.Mapping.Format v
                         >> CookieAttributes.Mapping.Format a

        { Parse = setCookieP
          Format = setCookieF }

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

and CookieAttribute =
    | Expires of DateTime
    | MaxAge of int
    | Domain of string
    | Path of string
    | Secure
    | HttpOnly

    static member internal Mapping =

        let cookieAttributeP =
                skipChar ';'
            >>. spP
            >>. choice [
                    skipString "Secure" >>% Secure ]

        let cookieAttributeF =
            function | Secure -> append "; Secure"
                     | _ -> id

        { Parse = cookieAttributeP
          Format = cookieAttributeF }

(* Cookie

   Taken from RFC 6265, Section 4.2 Cookie
   See [http://tools.ietf.org/html/rfc6265#section-4.2] *)

type Cookie =
    | Cookie of CookieName * CookieValue

    static member internal Mapping =

        let cookieP =
                CookieName.Mapping.Parse 
            .>> skipChar '='
           .>>. CookieValue.Mapping.Parse
            |>> Cookie

        let cookieF =
            function | Cookie(n, v) ->
                            CookieName.Mapping.Format n 
                         >> append "=" 
                         >> CookieValue.Mapping.Format v

        { Parse = cookieP
          Format = cookieF }

    static member Format =
        Formatting.format Cookie.Mapping.Format

    static member Parse =
        Parsing.parse Cookie.Mapping.Parse

    static member TryParse =
        Parsing.tryParse Cookie.Mapping.Parse

    override x.ToString () =
        Cookie.Format x