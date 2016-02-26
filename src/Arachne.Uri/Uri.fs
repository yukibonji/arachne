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

module Arachne.Uri

open System
open System.Text
open Arachne.Core
open FParsec

(* RFC 3986

   Types, parsers and formatters implemented to mirror the specification of 
   URI semantics as defined in RFC 3986.

   Taken from [http://tools.ietf.org/html/rfc3986] *)

(* Grammar *)

[<RequireQualifiedAccess>]
module Grammar =

    let isUnreserved i =
            Grammar.isAlpha i
         || Grammar.isDigit i
         || i = 0x2d // -
         || i = 0x2e // .
         || i = 0x5f // _
         || i = 0x7e // ~

    let isGenDelim i =
            i = 0x3a // :
         || i = 0x2f // /
         || i = 0x3f // ?
         || i = 0x23 // #
         || i = 0x5b // [
         || i = 0x5d // ]
         || i = 0x40 // @

    let isSubDelim i =
            i = 0x21 // !
         || i = 0x24 // $
         || i = 0x26 // &
         || i = 0x5c // \
         || i >= 0x28 && i <= 0x2c //   * + ,
         || i = 0x3b // ;
         || i = 0x3d // =

    let isReserved i=
            isGenDelim i
         || isSubDelim i

(* Aliases *)

module F = Formatting
module G = Grammar
module M = Mapping

(* IP Address *)

[<RequireQualifiedAccess>]
module IPAddress =

    let private isv6Char i =
            G.isHexdig i
         || i = 0x3a // :

    let private isv4Char i =
            G.isDigit i
         || i = 0x2e // .

    [<RequireQualifiedAccess>]
    module Format =

        let v6 x =
            F.append "[" >> F.append x >> F.append "]"

        let v4 x =
            F.append x

    [<RequireQualifiedAccess>]
    module Parse =

        let v6 =
            skipChar '[' >>. (many1Satisfy (int >> isv6Char) >>= (fun x ->
                match Uri.CheckHostName x with
                | UriHostNameType.IPv6 -> preturn x
                | _ -> pzero)) .>> skipChar ']'

        let v4 =
            many1Satisfy (int >> isv4Char) >>= (fun x ->
                match Uri.CheckHostName x with
                | UriHostNameType.IPv4 -> preturn x
                | _ -> pzero)

(* Percent-Encoding

   Code for percent-encoding data given some simple assumptions about what
   should be allowed through unencoded. *)

[<RequireQualifiedAccess>]
module PercentEncoding =

    (* Grammar *)

    let private pct =
        byte 0x25

    (* UTF-8

        Shorthand for UTF-8 encoding and decoding of strings (given
        the assumption that the .NET UTF-16/Unicode string is our
        basic string type). *)

    let private toBytes : string -> byte list =
        Encoding.UTF8.GetBytes >> List.ofArray

    let private toString : byte list -> string =
        List.toArray >> fun x -> Encoding.UTF8.GetString (x, 0, x.Length)

    (* Indices

        Simple lookups/indices for converting between bytes and the hex
        encoding of those bytes. *)

    let private toBytePair (s: string) =
        byte >> fun i -> i, toBytes (i.ToString s)

    let private index =
        let range = [ 0x00 .. 0xff ]
        let lower = List.map (toBytePair "x2") range
        let upper = List.map (toBytePair "X2") range
        
        lower @ upper

    let private byteIndex =
        index
        |> Map.ofList

    let private hexIndex =
        index
        |> List.map (fun (a, b) -> (b, a))
        |> Map.ofList

    (* Parsing

        Parsing functions, providing a function to create a parser
        given a whitelist of allowed characters within the input (pct-encoded
        values are implicitly allowed, and remain unaltered by the parser. *)

    let private pctP =
        tuple3 (pchar '%') hex hex 
        |>> (fun (p, a, b) ->
            [| p; a; b |])

    let makeParser (pred: int -> bool) =
        many (attempt pctP <|> (satisfy (int >> pred) |>> fun x -> [| x |]))
        |>> fun x ->
            new string (Array.concat x)

    (* Formatting

        Formatting functions, providing a function to create an formatter
        for a given string. No encoding is done as part of formatting, any
        characters within the provided string are assumed to be valid. *)

    let makeFormatter () =
        F.append

    (* Encoding

       Encoding functions, providing a function to create an encoder for
       a string given a whitelist of allowed characters within the input
       (non-whitelisted characters are pct-encoded automatically). *)

    let private hexdig =
        int >> G.isHexdig

    let private format p =
        let rec format r =
            function | [] -> r
                        | h :: x :: y :: t when h = pct && hexdig x && hexdig y -> format (r @ [ h; x; y ]) t
                        | h :: t when p (int h) -> format (r @ [ h ]) t
                        | h :: t -> format (r @ [ pct ] @ Map.find h byteIndex) t

        format []

    let makeEncoder (pred: int -> bool) =
        toBytes >> format pred >> toString

    (* Decoding

       Decoding functions, providing a function to create a decoder for a
       pct-encoded string, converting pct-encoded values to their Unicode/UTF-16
       form. *)

    let private pctDecodeP : Parser<char,unit> =
        pchar '%' >>. hex .>>. hex
        |>> fun (a, b) ->
            char (Map.find ([ byte a; byte b ]) hexIndex)

    let private decodeP =
        many (attempt pctDecodeP <|> anyChar)
        |>> fun x ->
            new string (Array.ofList x)

    let makeDecoder () =
        fun s ->
            match run decodeP s with
            | Success (s, _, _) -> s
            | _ -> failwith "Decode Failure"

(* Scheme

   Taken from RFC 3986, Section 3.1 Scheme
   See [http://tools.ietf.org/html/rfc3986#section-3.1] *)

(* Section 3.1 *)

type Scheme =
    | Scheme of string

    static member Mapping =

        let isSchemeChar i =
                G.isAlpha i
             || G.isDigit i
             || i = 0x2b // +
             || i = 0x2d // -
             || i = 0x2e // .

        let schemeP =
            satisfy (int >> G.isAlpha) .>>. manySatisfy (int >> isSchemeChar)
            |>> ((fun (x, xs) -> sprintf "%c%s" x xs) >> Scheme)

        let schemeF =
            function | Scheme x -> F.append x

        { Parse = schemeP
          Format = schemeF }

    (* Optics *)

    static member scheme_ =
        (fun (Scheme s) -> s), (Scheme)

    (* Common *)

    static member format =
        M.format Scheme.Mapping

    static member parse =
        M.parse Scheme.Mapping

    static member tryParse =
        M.tryParse Scheme.Mapping

    override x.ToString () =
        Scheme.format x

    (* Obsolete

       To be removed in 4.0. *)

    [<Obsolete ("Use scheme_ instead.")>]
    static member Scheme_ =
        Scheme.scheme_

    [<Obsolete ("Use format instead.")>]
    static member Format =
        Scheme.format

    [<Obsolete ("Use parse instead.")>]
    static member Parse =
        Scheme.parse

    [<Obsolete ("Use tryParse instead.")>]
    static member TryParse =
        Scheme.tryParse

(* Authority

   Taken from RFC 3986, Section 3.2 Authority
   See [http://tools.ietf.org/html/rfc3986#section-3.2] *)

(* Section 3.2 *)

type Authority =
    | Authority of Host * Port option * UserInfo option

    static member Mapping =

        let authorityP =
                 opt (attempt (UserInfo.Mapping.Parse .>> skipChar '@')) 
            .>>. Host.Mapping.Parse
            .>>. opt Port.Mapping.Parse
             |>> fun ((user, host), port) -> Authority (host, port, user)

        let authorityF =
            function | Authority (h, p, u) ->
                        let formatters =
                            [ (function | Some u -> UserInfo.Mapping.Format u >> F.append "@"
                                        | _ -> id) u
                              Host.Mapping.Format h
                              (function | Some p -> Port.Mapping.Format p 
                                        | _ -> id) p ]

                        fun b -> List.fold (|>) b formatters

        { Parse = authorityP
          Format = authorityF }

    (* Optics *)

    static member host_ =
        (fun (Authority (h, _, _)) -> h),
        (fun h (Authority (_, p, u)) -> Authority (h, p, u))

    static member port_ =
        (fun (Authority (_, p, _)) -> p),
        (fun p (Authority (h, _, u)) -> Authority (h, Some p, u))

    static member userInfo_ =
        (fun (Authority (_, _, u)) -> u),
        (fun u (Authority (h, p, _)) -> Authority (h, p, Some u))

    (* Common *)

    static member format =
        M.format Authority.Mapping

    static member parse =
        M.parse Authority.Mapping
    
    static member tryParse =
        M.tryParse Authority.Mapping

    override x.ToString () =
        Authority.format x

    (* Obsolete

       To be removed in 4.0. *)

    [<Obsolete ("Use host_ instead.")>]
    static member Host_ =
        Authority.host_

    [<Obsolete ("Use port_ instead.")>]
    static member Port_ =
        Authority.port_

    [<Obsolete ("Use userInfo_ instead.")>]
    static member UserInfo_ =
        Authority.userInfo_

    [<Obsolete ("Use format instead.")>]
    static member Format =
        Authority.format

    [<Obsolete ("Use parse instead.")>]
    static member Parse =
        Authority.parse

    [<Obsolete ("Use tryParse instead.")>]
    static member TryParse =
        Authority.tryParse

(* Section 3.2.1 *)

 and UserInfo =
    | UserInfo of string

    static member private Predicate i =
            G.isUnreserved i
         || G.isSubDelim i
         || i = 0x3a // :

    static member Mapping =

        let parser =
            PercentEncoding.makeParser UserInfo.Predicate

        let formatter =
            PercentEncoding.makeFormatter ()

        let userInfoP =
            notEmpty parser |>> UserInfo

        let userInfoF =
            function | UserInfo x -> formatter x

        { Parse = userInfoP
          Format = userInfoF }

    (* Optics *)

    static member raw_ =
        (fun (UserInfo u) -> u), (UserInfo)

    static member decoded_ =
        
        let encoder =
            PercentEncoding.makeEncoder UserInfo.Predicate

        let decoder =
            PercentEncoding.makeDecoder ()

        (fun (UserInfo u) -> decoder u), (encoder >> UserInfo)

(* Section 3.2.2 *)

(* Note: In this instance we use the built in IP Address type and parser
   as the standard is implemented fully and seems to handle all suitable cases.

   We also make a slight restriction for practicality at the moment on
   implementing IP-literal as an IPv6 specific type, discarding IPvFuture. As
   it stands, that's unlikely to be an issue, but could perhaps be revisited. *)

 and Host =
    | IPv4 of string
    | IPv6 of string
    | Name of RegName

    static member Mapping =

        let hostP =
            choice [
                IPAddress.Parse.v4 |>> IPv4
                IPAddress.Parse.v6 |>> IPv6
                RegName.Mapping.Parse |>> Name ]

        let hostF =
            function | IPv4 x -> IPAddress.Format.v4 x
                     | IPv6 x -> IPAddress.Format.v6 x
                     | Name x -> RegName.Mapping.Format x

        { Parse = hostP
          Format = hostF }

    (* Optics *)

    static member ipv4_ =
        (function | IPv4 i -> Some i | _ -> None), (IPv4)

    static member ipv6_ =
        (function | IPv6 i -> Some i | _ -> None), (IPv6)

    static member name_ =
        (function | Name n -> Some n | _ -> None), (Name)

    (* Obsolete

       To be removed in 4.0. *)

    [<Obsolete ("Use ipv4_ instead.")>]
    static member IPv4_ =
        Host.ipv4_

    [<Obsolete ("Use ipv6_ instead.")>]
    static member IPv6_ =
        Host.ipv6_

    [<Obsolete ("Use name_ instead.")>]
    static member Name_ =
        Host.name_


 and RegName =
    | RegName of string

    static member private Predicate  i =
            G.isUnreserved i
         || G.isSubDelim i

    static member Mapping =

        let parser =
            PercentEncoding.makeParser RegName.Predicate

        let formatter =
            PercentEncoding.makeFormatter ()

        let regNameP =
            parser |>> RegName

        let regNameF =
            function | RegName x -> formatter x

        { Parse = regNameP
          Format = regNameF }

    (* Optics *)

    static member raw_ =
        (fun (RegName n) -> n), (RegName)

    static member decoded_ =

        let encoder =
            PercentEncoding.makeEncoder RegName.Predicate

        let decoder =
            PercentEncoding.makeDecoder ()

        (fun (RegName n) -> decoder n), (encoder >> RegName)

 and Port =
    | Port of int

    static member Mapping =

        let portP =
                skipChar ':' >>. puint32 
            |>> (int >> Port)

        let portF =
            function | Port x -> F.append ":" >> F.append (string x)

        { Parse = portP
          Format = portF }

    (* Optics *)

    static member port_ =
        (fun (Port p) -> p), (Port)

    (* Obsolete

       To be removed in 4.0. *)

    [<Obsolete ("Use port_ instead.")>]
    static member Port_ =
        Port.port_

(* Path

   Taken from RFC 3986, Section 3.3 Path
   See [http://tools.ietf.org/html/rfc3986#section-3.3] *)

[<AutoOpen>]
module private Path =

    let isPcharNc i =
            G.isUnreserved i
         || G.isSubDelim i
         || i = 0x40 // @

    let pcharNcP =
        PercentEncoding.makeParser isPcharNc

    let pcharNcE =
        PercentEncoding.makeEncoder isPcharNc

    let isPchar i =
            isPcharNc i
         || i = 0x3a // :

    let pcharP =
        PercentEncoding.makeParser isPchar

    let pcharF =
        PercentEncoding.makeFormatter ()

    let pcharE =
        PercentEncoding.makeEncoder isPchar

    let pcharD =
        PercentEncoding.makeDecoder ()

(* Absolute Or Empty *)

type PathAbsoluteOrEmpty =
    | PathAbsoluteOrEmpty of string list

    static member Mapping =

        let pathAbsoluteOrEmptyP =
                many (skipChar '/' >>. pcharP) 
            |>> PathAbsoluteOrEmpty

        let pathAbsoluteOrEmptyF =
            function | PathAbsoluteOrEmpty [] -> id
                     | PathAbsoluteOrEmpty xs -> F.append "/" >> F.join pcharF (F.append "/") xs

        { Parse = pathAbsoluteOrEmptyP
          Format = pathAbsoluteOrEmptyF }

    (* Optics *)

    static member raw_ =
        (fun (PathAbsoluteOrEmpty p) -> p), (PathAbsoluteOrEmpty)

    static member decoded_ =
        (fun (PathAbsoluteOrEmpty p) -> List.map pcharD p),
        (List.map pcharE >> PathAbsoluteOrEmpty)

    (* Common *)

    static member format =
        M.format PathAbsoluteOrEmpty.Mapping

    static member parse =
        M.parse PathAbsoluteOrEmpty.Mapping

    static member tryParse =
        M.tryParse PathAbsoluteOrEmpty.Mapping

    override x.ToString () =
        PathAbsoluteOrEmpty.format x

    (* Obsolete

       To be removed in 4.0. *)

    [<Obsolete ("Use format instead.")>]
    static member Format =
        PathAbsoluteOrEmpty.format

    [<Obsolete ("Use parse instead.")>]
    static member Parse =
        PathAbsoluteOrEmpty.parse

    [<Obsolete ("Use tryParse instead.")>]
    static member TryParse =
        PathAbsoluteOrEmpty.tryParse

(* Absolute *)

type PathAbsolute =
    | PathAbsolute of string list

    static member Mapping =

        let pathAbsoluteP =
            skipChar '/' >>. opt (notEmpty pcharP .>>. many (skipChar '/' >>. pcharP))
            |>> function | Some (x, xs) -> PathAbsolute (x :: xs)
                         | _ -> PathAbsolute []

        let pathAbsoluteF =
            function | PathAbsolute xs -> F.append "/" >> F.join pcharF (F.append "/") xs

        { Parse = pathAbsoluteP
          Format = pathAbsoluteF }

    (* Optics *)

    static member raw_ =
        (fun (PathAbsolute p) -> p), (PathAbsolute)

    static member decoded_ =
        (fun (PathAbsolute p) -> List.map pcharD p),
        (List.map pcharE >>PathAbsolute)

    (* Common *)

    static member format =
        M.format PathAbsolute.Mapping

    static member parse =
        M.parse PathAbsolute.Mapping

    static member tryParse =
        M.tryParse PathAbsolute.Mapping

    override x.ToString () =
        PathAbsolute.format x

    (* Obsolete

       To be removed in 4.0. *)

    [<Obsolete ("Use format instead.")>]
    static member Format =
        PathAbsolute.format

    [<Obsolete ("Use parse instead.")>]
    static member Parse =
        PathAbsolute.parse

    [<Obsolete ("Use tryParse instead.")>]
    static member TryParse =
        PathAbsolute.tryParse

(* No Scheme *)

type PathNoScheme =
    | PathNoScheme of string list

    static member Mapping =

        let pathNoSchemeP =
                 notEmpty pcharNcP
            .>>. many (skipChar '/' >>. pcharP)
             |>> fun (x, xs) -> PathNoScheme (x :: xs)

        let pathNoSchemeF =
            function | PathNoScheme xs -> F.join pcharF (F.append "/") xs

        { Parse = pathNoSchemeP
          Format = pathNoSchemeF }

    (* Optics *)

    static member raw_ =
        (fun (PathNoScheme p) -> p), (PathNoScheme)

    static member decoded_ =
        (fun (PathNoScheme p) -> List.map pcharD p),
        (List.map pcharNcE >> PathNoScheme)

    (* Common *)

    static member format =
        M.format PathNoScheme.Mapping

    static member parse =
        M.parse PathNoScheme.Mapping

    static member tryParse =
        M.tryParse PathNoScheme.Mapping

    override x.ToString () =
        PathNoScheme.format x

    (* Obsolete

       To be removed in 4.0. *)

    [<Obsolete ("Use format instead.")>]
    static member Format =
        PathNoScheme.format

    [<Obsolete ("Use parse instead.")>]
    static member Parse =
        PathNoScheme.parse

    [<Obsolete ("Use tryParse instead.")>]
    static member TryParse =
        PathNoScheme.tryParse

(* Rootless *)

type PathRootless =
    | PathRootless of string list

    static member Mapping =

        let pathRootlessP =
                 notEmpty pcharP
            .>>. many (skipChar '/' >>. pcharP)
             |>> fun (x, xs) -> PathRootless (x :: xs)

        let pathRootlessF =
            function | PathRootless xs -> F.join pcharF (F.append "/") xs

        { Parse = pathRootlessP
          Format = pathRootlessF }

    (* Optics *)

    static member raw_ =
        (fun (PathRootless p) -> p), (PathRootless)

    static member decoded_ =
        (fun (PathRootless p) -> List.map pcharD p),
        (List.map pcharE >> PathRootless)

    (* Common *)

    static member format =
        M.format PathRootless.Mapping

    static member parse =
        M.parse PathRootless.Mapping

    static member tryParse =
        M.tryParse PathRootless.Mapping

    override x.ToString () =
        PathRootless.format x

    (* Obsolete

       To be removed in 4.0. *)

    [<Obsolete ("Use format instead.")>]
    static member Format =
        PathRootless.format

    [<Obsolete ("Use parse instead.")>]
    static member Parse =
        PathRootless.parse

    [<Obsolete ("Use tryParse instead.")>]
    static member TryParse =
        PathRootless.tryParse

(* Query

   Taken from RFC 3986, Section 3.4 Query
   See [http://tools.ietf.org/html/rfc3986#section-3.4] *)

type Query =
    | Query of string

    static member private Predicate i =
            isPchar i
         || i = 0x2f // /
         || i = 0x3f // ?

    static member Mapping =

        let parser =
            PercentEncoding.makeParser Query.Predicate

        let formatter =
            PercentEncoding.makeFormatter ()

        let queryP =
            parser |>> Query

        let queryF =
            function | Query x -> formatter x

        { Parse = queryP
          Format = queryF }

    (* Optics *)

    static member raw_ =
        (fun (Query q) -> q), (Query)

    static member decoded_ =

        let encoder =
            PercentEncoding.makeEncoder Query.Predicate

        let decoder =
            PercentEncoding.makeDecoder ()

        (fun (Query q) -> decoder q), (encoder >> Query)

    // TODO: Review the Pairs optics...

    static member pairs_ =

        let isEqualsOrAmpersand i =
                i = 0x3d // =
             || i = 0x26 // &

        let pairPartV =
            manySatisfy (int >> isEqualsOrAmpersand >> not)

        let pairPartP =
            manySatisfy (int >> isEqualsOrAmpersand >> not)

        let pairP =
            pairPartP .>>. opt(( skipChar '=') >>. ( pairPartV))
        
        let skipAmp = skipChar '&'

        let pairsP =
            sepBy1 pairP skipAmp
        
        let pairF =
            function | (k, Some v) -> F.append k >> F.append "=" >> F.append v
                     | (k, None) -> F.append k

        let pairsF =
            F.join pairF (F.append "&")

        (fun (Query q) ->
            match q with
            | q when String.IsNullOrWhiteSpace q -> None
            | q ->
                match run pairsP q with
                | Success (x, _, _) -> Some x
                | Failure (_) -> None),
        ((fun a -> string (pairsF a (StringBuilder ()))) >> Query)

    (* Common *)

    static member format =
        M.format Query.Mapping

    static member parse =
        M.parse Query.Mapping

    static member tryParse =
        M.tryParse Query.Mapping

    override x.ToString () =
        Query.format x

    (* Obsolete

       To be removed in 4.0. *)

    [<Obsolete ("User pairs_ instead.")>]
    static member Pairs_ =
        Query.pairs_

    [<Obsolete ("Use format instead.")>]
    static member Format =
        Query.format

    [<Obsolete ("Use parse instead.")>]
    static member Parse =
        Query.parse

    [<Obsolete ("Use tryParse instead.")>]
    static member TryParse =
        Query.tryParse

(* Fragment

   Taken from RFC 3986, Section 3.5 Fragment
   See [http://tools.ietf.org/html/rfc3986#section-3.5] *)

type Fragment =
    | Fragment of string

    static member private Predicate i =
                isPchar i
             || i = 0x2f // /
             || i = 0x3f // ?

    static member Mapping =

        let parser =
            PercentEncoding.makeParser Fragment.Predicate

        let formatter =
            PercentEncoding.makeFormatter ()

        let fragmentP =
            parser |>> Fragment

        let fragmentF =
            function | Fragment x -> formatter x

        { Parse = fragmentP
          Format = fragmentF }

    (* Optics*)

    static member raw_ =
        (fun (Fragment f) -> f), (Fragment)

    static member decoded_ =

        let encoder =
            PercentEncoding.makeEncoder Fragment.Predicate

        let decoder =
            PercentEncoding.makeDecoder ()

        (fun (Fragment f) -> decoder f), (encoder >> Fragment)

    (* Common *)

    static member format =
        M.format Fragment.Mapping

    static member parse =
        M.parse Fragment.Mapping

    static member tryParse =
        M.tryParse Fragment.Mapping

    override x.ToString () =
        Fragment.format x

    (* Obsolete

       To be removed in 4.0. *)

    [<Obsolete ("Use format instead.")>]
    static member Format =
        Fragment.format

    [<Obsolete ("Use parse instead.")>]
    static member Parse =
        Fragment.parse

    [<Obsolete ("Use tryParse instead.")>]
    static member TryParse =
        Fragment.tryParse

(* URI

   Taken from RFC 3986, Section 3 URI
   See [http://tools.ietf.org/html/rfc3986#section-3] *)

(* Note: In the case of absolute paths in the hierarchy, which the parser
   will correctly determine, the type system cannot adequately protect
   against an absolute path being created with two initial empty
   segments (and in the absence of a strong dependent system, it's likely
   to stay that way without extreme convolution). (Created in this case
   refers to direct instance creation).

   It is therefore possible to create an invalid URI string using the URI
   type if some care is not taken. For now this will simply have to stand
   as an allowed but "known not ideal" behaviour, under review.
   
   It is of course also possible to create invalid paths by creating strings
   which are invalid segments. Though the parser will reject these, manual
   creation will still allow this case. *)

type Uri =
    | Uri of Scheme * HierarchyPart * Query option * Fragment option

    static member Mapping =

        let uriP =
                 Scheme.Mapping.Parse .>> skipChar ':'
            .>>. HierarchyPart.Mapping.Parse 
            .>>. opt (skipChar '?' >>. Query.Mapping.Parse)
            .>>. opt (skipChar '#' >>. Fragment.Mapping.Parse)
             |>> fun (((scheme, hierarchy), query), fragment) ->
                Uri (scheme, hierarchy, query, fragment)

        let uriF =
            function | Uri (s, h, q, f) -> 
                        let formatters =
                            [ Scheme.Mapping.Format s
                              F.append ":"
                              HierarchyPart.Mapping.Format h
                              (function | Some q -> F.append "?" >> Query.Mapping.Format q 
                                        | _ -> id) q
                              (function | Some f -> F.append "#" >> Fragment.Mapping.Format f 
                                        | _ -> id) f ]

                        fun b -> List.fold (|>) b formatters

        { Parse = uriP
          Format = uriF }

    (* Optics *)

    static member scheme_ =
        (fun (Uri (s, _, _, _)) -> s),
        (fun s (Uri (_, h, q, f)) -> Uri (s, h, q, f))

    static member hierarchyPart_ =
        (fun (Uri (_, h, _, _)) -> h),
        (fun h (Uri (s, _, q, f)) -> Uri (s, h, q, f))

    static member query_ =
        (fun (Uri (_, _, q, _)) -> q),
        (fun q (Uri (s, h, _, f)) -> Uri (s, h, Some q, f))

    static member fragment_ =
        (fun (Uri (_, _, _, f)) -> f),
        (fun f (Uri (s, h, q, _)) -> Uri (s, h, q, Some f))

    (* Common *)

    static member format =
        M.format Uri.Mapping

    static member parse =
        M.parse Uri.Mapping

    static member tryParse =
        M.tryParse Uri.Mapping

    override x.ToString () =
        Uri.format x

    (* Obsolete

       To be removed in 4.0. *)

    [<Obsolete ("Use scheme_ instead.")>]
    static member Scheme_ =
        Uri.scheme_

    [<Obsolete ("Use hierarchyPart_ instead.")>]
    static member HierarchyPart_ =
        Uri.hierarchyPart_

    [<Obsolete ("Use query_ instead.")>]
    static member Query_ =
        Uri.query_

    [<Obsolete ("Use fragment_ instead.")>]
    static member Fragment_ =
        Uri.fragment_

    [<Obsolete ("Use format instead.")>]
    static member Format =
        Uri.format

    [<Obsolete ("Use parse instead.")>]
    static member Parse =
        Uri.parse

    [<Obsolete ("Use tryParse instead.")>]
    static member TryParse =
        Uri.tryParse

 and HierarchyPart =
    | Authority of Authority * PathAbsoluteOrEmpty
    | Absolute of PathAbsolute
    | Rootless of PathRootless
    | Empty

    static member Mapping =

        let authorityP =
                 skipString "//" >>. Authority.Mapping.Parse 
            .>>. PathAbsoluteOrEmpty.Mapping.Parse 
             |>> Authority

        let hierarchyPartP =
            choice [
                authorityP
                PathAbsolute.Mapping.Parse |>> Absolute
                PathRootless.Mapping.Parse |>> Rootless
                preturn Empty ]

        let authorityF (a, p) =
                F.append "//" 
             >> Authority.Mapping.Format a 
             >> PathAbsoluteOrEmpty.Mapping.Format p

        let hierarchyPartF =
            function | Authority (a, p) -> authorityF (a, p)
                     | Absolute p -> PathAbsolute.Mapping.Format p
                     | Rootless p -> PathRootless.Mapping.Format p
                     | Empty -> id

        { Parse = hierarchyPartP
          Format = hierarchyPartF }

    (* Optics *)

    static member authority_ =
        (function | Authority (a, p) -> Some (a, p) | _ -> None), (fun (a, p) -> Authority (a, p))

    static member absolute_ =
        (function | Absolute p -> Some p | _ -> None), (Absolute)

    static member rootless_ =
        (function | Rootless p -> Some p | _ -> None), (Rootless)

    static member empty_ =
        (function | Empty -> Some () | _ -> None), (fun () -> Empty)

    (* Obsolete

       To be removed in 4.0. *)

    [<Obsolete ("Use authority_ instead.")>]
    static member Authority_ =
        HierarchyPart.authority_

    [<Obsolete ("Use absolute_ instead.")>]
    static member Absolute_ =
        HierarchyPart.absolute_

    [<Obsolete ("Use rootless_ instead.")>]
    static member Rootless_ =
        HierarchyPart.rootless_

    [<Obsolete ("Use empty_ instead.")>]
    static member Empty_ =
        HierarchyPart.empty_

(* Relative Reference

   Taken from RFC 3986, Section 4.2 Relative Reference
   See [http://tools.ietf.org/html/rfc3986#section-4.2] *)

type RelativeReference =
    | RelativeReference of RelativePart * Query option * Fragment option

    static member Mapping =

        let relativeReferenceP =
                 RelativePart.Mapping.Parse
            .>>. opt (skipChar '?' >>. Query.Mapping.Parse)
            .>>. opt (skipChar '#' >>. Fragment.Mapping.Parse)
             |>> fun ((relative, query), fragment) ->
                RelativeReference (relative, query, fragment)

        let relativeReferenceF =
            function | RelativeReference (r, q, f) -> 
                        let formatters =
                            [ RelativePart.Mapping.Format r
                              (function | Some q -> F.append "?" >> Query.Mapping.Format q
                                        | _ -> id) q
                              (function | Some f -> F.append "#" >> Fragment.Mapping.Format f
                                        | _ -> id) f ]

                        fun b -> List.fold (|>) b formatters

        { Parse = relativeReferenceP
          Format = relativeReferenceF }

    (* Optics *)

    static member relativePart_ =
        (fun (RelativeReference (r, _, _)) -> r),
        (fun r (RelativeReference (_, q, f)) -> RelativeReference (r, q, f))

    static member query_ =
        (fun (RelativeReference (_, q, _)) -> q),
        (fun q (RelativeReference (r, _, f)) -> RelativeReference (r, Some q, f))

    static member fragment_ =
        (fun (RelativeReference (_, _, f)) -> f),
        (fun f (RelativeReference (r, q, _)) -> RelativeReference (r, q, Some f))

    (* Common *)

    static member format =
        M.format RelativeReference.Mapping

    static member parse =
        M.parse RelativeReference.Mapping

    static member tryParse =
        M.tryParse RelativeReference.Mapping

    override x.ToString () =
        RelativeReference.format x

    (* Obsolete

       To be removed in 4.0. *)

    [<Obsolete ("Use relativePart_ instead.")>]
    static member RelativePart_ =
        RelativeReference.relativePart_

    [<Obsolete ("Use query_ instead.")>]
    static member Query_ =
        RelativeReference.query_

    [<Obsolete ("Use fragment_ instead.")>]
    static member Fragment_ =
        RelativeReference.fragment_

    [<Obsolete ("Use format instead.")>]
    static member Format =
        RelativeReference.format

    [<Obsolete ("Use parse instead.")>]
    static member Parse =
        RelativeReference.parse

    [<Obsolete ("Use tryParse instead.")>]
    static member TryParse =
        RelativeReference.tryParse

 and RelativePart =
    | Authority of Authority * PathAbsoluteOrEmpty
    | Absolute of PathAbsolute
    | NoScheme of PathNoScheme
    | Empty

    static member Mapping =

        let authorityP =
                 skipString "//" >>. Authority.Mapping.Parse
            .>>. PathAbsoluteOrEmpty.Mapping.Parse 
             |>> Authority

        let relativePartP =
            choice [
                authorityP
                PathAbsolute.Mapping.Parse |>> Absolute
                PathNoScheme.Mapping.Parse |>> NoScheme
                preturn Empty ]

        let authorityF (a, p) =
                F.append "//" 
             >> Authority.Mapping.Format a 
             >> PathAbsoluteOrEmpty.Mapping.Format p

        let relativePartF =
            function | Authority (a, p) -> authorityF (a, p)
                     | Absolute p -> PathAbsolute.Mapping.Format p
                     | NoScheme p -> PathNoScheme.Mapping.Format p
                     | Empty -> id

        { Parse = relativePartP
          Format = relativePartF }

    (* Optics *)

    static member authority_ =
        (function | Authority (a, p) -> Some (a, p) | _ -> None), (fun (a, p) -> Authority (a, p))

    static member absolute_ =
        (function | Absolute p -> Some p | _ -> None), (Absolute)

    static member noScheme_ =
        (function | NoScheme p -> Some p | _ -> None), (NoScheme)

    static member empty_ =
        (function | Empty -> Some () | _ -> None), (fun () -> Empty)

    (* Obsolete

       To be removed in 4.0. *)

    [<Obsolete ("Use authority_ instead.")>]
    static member Authority_ =
        RelativePart.authority_

    [<Obsolete ("Use absolute_ instead.")>]
    static member Absolute_ =
        RelativePart.absolute_

    [<Obsolete ("Use noScheme_ instead.")>]
    static member NoScheme_ =
        RelativePart.noScheme_

    [<Obsolete ("Use empty_ instead.")>]
    static member Empty_ =
        RelativePart.empty_

(* Absolute URI

   Taken from RFC 3986, Section 4.3 Absolute URI
   See [http://tools.ietf.org/html/rfc3986#section-4.3] *)

type AbsoluteUri =
    | AbsoluteUri of Scheme * HierarchyPart * Query option

    static member Mapping =

        let absoluteUriP =
                 Scheme.Mapping.Parse .>> skipChar ':' 
            .>>. HierarchyPart.Mapping.Parse 
            .>>. opt (skipChar '?' >>. Query.Mapping.Parse)
             |>> fun ((scheme, hierarchy), query) ->
                AbsoluteUri (scheme, hierarchy, query)

        let absoluteUriF =
            function | AbsoluteUri (s, h, q) -> 
                        let formatters =
                            [ Scheme.Mapping.Format s
                              F.append ":"
                              HierarchyPart.Mapping.Format h
                              (function | Some q -> F.append "?" >> Query.Mapping.Format q
                                        | _ -> id) q ]

                        fun b -> List.fold (|>) b formatters

        { Parse = absoluteUriP
          Format = absoluteUriF }

    (* Optics *)

    static member scheme_ =
        (fun (AbsoluteUri (s, _, _)) -> s),
        (fun s (AbsoluteUri (_, h, q)) -> AbsoluteUri (s, h, q))

    static member hierarchyPart_ =
        (fun (AbsoluteUri (_, h, _)) -> h),
        (fun h (AbsoluteUri (s, _, q)) -> AbsoluteUri (s, h, q))

    static member query_ =
        (fun (AbsoluteUri (_, _, q)) -> q),
        (fun q (AbsoluteUri (s, h, _)) -> AbsoluteUri (s, h, Some q))

    (* Common *)

    static member format =
        M.format AbsoluteUri.Mapping

    static member parse =
        M.parse AbsoluteUri.Mapping

    static member tryParse =
        M.tryParse AbsoluteUri.Mapping

    override x.ToString () =
        AbsoluteUri.format x

    (* Obsolete

       To be removed in 4.0. *)

    [<Obsolete ("Use scheme_ instead.")>]
    static member Scheme_ =
        AbsoluteUri.scheme_

    [<Obsolete ("Use hierarchyPart_ instead.")>]
    static member HierarchyPart_ =
        AbsoluteUri.hierarchyPart_

    [<Obsolete ("Use query_ instead.")>]
    static member Query_ =
        AbsoluteUri.query_

    [<Obsolete ("Use format instead.")>]
    static member Format =
        AbsoluteUri.format

    [<Obsolete ("Use parse instead.")>]
    static member Parse =
        AbsoluteUri.parse

    [<Obsolete ("Use tryParse instead.")>]
    static member TryParse =
        AbsoluteUri.tryParse

(* URI Reference

   Taken from RFC 3986, Section 4.1 URI Reference
   See [http://tools.ietf.org/html/rfc3986#section-4.1] *)

type UriReference =
    | Uri of Uri
    | Relative of RelativeReference

    static member Mapping =

        let uriReferenceP =
            choice [
                attempt Uri.Mapping.Parse |>> Uri
                RelativeReference.Mapping.Parse |>> Relative ]

        let uriReferenceF =
            function | Uri x -> Uri.Mapping.Format x
                     | Relative x -> RelativeReference.Mapping.Format x

        { Parse = uriReferenceP
          Format = uriReferenceF }

    (* Optics *)

    static member uri_ =
        (function | Uri u -> Some u | _ -> None), (Uri)

    static member relative_ =
        (function | Relative r -> Some r | _ -> None), (Relative)

    (* Common *)

    static member format =
        M.format UriReference.Mapping

    static member parse =
        M.parse UriReference.Mapping

    static member tryParse =
        M.tryParse UriReference.Mapping

    override x.ToString () =
        UriReference.format x

    (* Obsolete

       To be removed in 4.0. *)

    [<Obsolete ("Use uri_ instead.")>]
    static member Uri_ =
        UriReference.uri_

    [<Obsolete ("Use relative_ instead.")>]
    static member Relative_ =
        UriReference.relative_

    [<Obsolete ("Use format instead.")>]
    static member Format =
        UriReference.format

    [<Obsolete ("Use parse instead.")>]
    static member Parse =
        UriReference.parse

    [<Obsolete ("Use tryParse instead.")>]
    static member TryParse =
        UriReference.tryParse