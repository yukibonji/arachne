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

module Arachne.Http

open System
open System.Globalization
open Arachne.Core
open Arachne.Language
open Arachne.Uri
open FParsec

(* RFC 7230

   Types, parsers and formatters implemented to mirror the specification of 
   HTTP semantics as defined in RFC 7230.

   Taken from [http://tools.ietf.org/html/rfc7230] *)

(* Grammar *)

[<RequireQualifiedAccess>]
module Grammar =

    (* Field Value Components

       Taken from RFC 7230, Section 3.2.6. Field Value Components
       See [http://tools.ietf.org/html/rfc7230#section-3.2.6] *)

    let isTchar i =
            Grammar.isAlpha i
         || Grammar.isDigit i
         || i = 0x21 // !
         || i >= 0x23 && i <= 0x26 // # $ % &
         || i = 0x5c // \
         || i = 0x2a // *
         || i = 0x2b // +
         || i = 0x2d // -
         || i = 0x2e // .
         || i = 0x5e // ^
         || i = 0x5f // _
         || i = 0x60 // `
         || i = 0x7c // |
         || i = 0x7e // ~

    let isObstext i =
            i >= 0x80 && i <= 0xff

    let isQdtext i =
            Grammar.isHtab i
         || Grammar.isSp i
         || i = 0x21
         || i >= 0x23 && i <= 0x5b
         || i >= 0x5d && i <= 0x7e
         || isObstext i

    let isQuotedPairChar i =
            Grammar.isHtab i
         || Grammar.isSp i
         || Grammar.isVchar i
         || isObstext i

    [<RequireQualifiedAccess>]
    module Parse =

        (* Whitespace

           Taken from RFC 7230, Section 3.2.3. Whitespace
           See [http://tools.ietf.org/html/rfc7230#section-3.2.3] *)

        let ows = 
            skipManySatisfy (int >> Grammar.isWsp)

        let token = 
            many1Satisfy (int >> isTchar)

        let quotedPair : Parser<char, unit> =
                skipChar '\\' 
            >>. satisfy (int >> isQuotedPairChar)

        let quotedString : Parser<string, unit> =
                skipSatisfy (int >> Grammar.isDquote)
            >>. many (quotedPair <|> satisfy (int >> isQdtext)) |>> (fun x -> string (System.String (List.toArray x)))
            .>> skipSatisfy (int >> Grammar.isDquote)

        (* ABNF List Extension: #rule

           Taken from RFC 7230, Section 7. ABNF List Extension: #rule
           [http://tools.ietf.org/html/rfc7230#section-7] *)

        let infixHead p s =
            (attempt p |>> Some) <|> (s >>% None)

        let infixTail p s =
            many (ows >>? s >>? ows >>? opt p)

        (* Note:
           The infix and prefix parsers are designed to convey as accurately as possible the 
           meaning of the ABNF #rule extension including the laxity of specification for backward 
           compatibility. Whether they are a perfectly true representation is open to debate, 
           but they should perform sensibly under normal conditions. *)

        let infix p s = 
            infixHead p s .>>. infixTail p s .>> ows |>> fun (x, xs) -> x :: xs |> List.choose id

        let infix1 p s =
            notEmpty (infix p s)

        let prefix p s =
            many (ows >>? s >>? ows >>? p)

(* Aliases *)

module F = Formatting
module G = Grammar
module M = Mapping

(* Uniform Resource Identifiers

   Taken from RFC 7230, Section 2.7 Uniform Resource Identifiers
   See [http://tools.ietf.org/html/rfc7230#section-3.2.3] *)

type PartialUri =
    | PartialUri of RelativePart * Query option

    static member Mapping =

        let partialUriP =
            RelativePart.Mapping.Parse .>>. opt (skipChar '?' >>. Query.Mapping.Parse)
            |>> PartialUri

        let partialUriF =
            function | PartialUri (r, q) ->
                        let formatters =
                            [ RelativePart.Mapping.Format r
                              (function | Some q -> F.append "?" >> Query.Mapping.Format q 
                                        | _ -> id) q ]

                        fun b -> List.fold (|>) b formatters

        { Parse = partialUriP
          Format = partialUriF }

    static member format =
        M.format PartialUri.Mapping

    static member parse =
        M.parse PartialUri.Mapping

    static member tryParse =
        M.tryParse PartialUri.Mapping

    override x.ToString () =
        PartialUri.format x

(* HTTP Version

   Taken from RFC 7230, Section 3.1 Request Line
   See [http://tools.ietf.org/html/rfc7230#section-3.1] *)

type HttpVersion =
    | HTTP of float 
    | Custom of string

    static member Mapping =

        let httpVersionP =
            choice [
                skipString "HTTP/1.0" >>% HttpVersion.HTTP 1.0
                skipString "HTTP/1.1" >>% HttpVersion.HTTP 1.1
                restOfLine false |>> HttpVersion.Custom ]

        let httpVersionF =
            function | HttpVersion.HTTP x -> F.appendf1 "HTTP/{0:G4}" x 
                     | HttpVersion.Custom x -> F.append x

        { Parse = httpVersionP
          Format = httpVersionF }

    static member format =
        M.format HttpVersion.Mapping

    static member parse =
        M.parse HttpVersion.Mapping

    static member tryParse =
        M.tryParse HttpVersion.Mapping

    override x.ToString () =
        HttpVersion.format x

(* Content-Length

   Taken from RFC 7230, Section 3.3.2 Content-Length
   See [http://tools.ietf.org/html/rfc7230#section-3.3.2] *)

type ContentLength =
    | ContentLength of int

    static member Mapping =

        let contentLengthP =
            puint32 |>> (int >> ContentLength)

        let contentLengthF =
            function | ContentLength x -> F.append (string x)

        { Parse = contentLengthP
          Format = contentLengthF }

    static member format =
        M.format ContentLength.Mapping

    static member parse =
        M.parse ContentLength.Mapping

    static member tryParse =
        M.tryParse ContentLength.Mapping

    override x.ToString () =
        ContentLength.format x

(* Host

   Taken from RFC 7230, Section 5.4 Host
   See [http://tools.ietf.org/html/rfc7230#section-5.4] *)

type Host =
    | Host of Arachne.Uri.Host * Port option

    static member Mapping =

        let hostP =
            Arachne.Uri.Host.Mapping.Parse .>>. opt Port.Mapping.Parse |>> Host

        let hostF =
            function | Host (h, p) ->
                        let formatters =
                            [ Arachne.Uri.Host.Mapping.Format h
                              (function | Some p -> Port.Mapping.Format p
                                        | _ -> id) p ]

                        fun b -> List.fold (|>) b formatters

        { Parse = hostP
          Format = hostF }

    static member format =
        M.format Host.Mapping

    static member parse =
        M.parse Host.Mapping

    static member tryParse =
        M.tryParse Host.Mapping

    override x.ToString () =
        Host.format x

(* Connection

   Taken from RFC 7230, Section 6.1 Connection
   See [http://tools.ietf.org/html/rfc7230#section-6.1] *)

type Connection =
    | Connection of ConnectionOption list

    static member Mapping =

        let connectionP =
            G.Parse.infix G.Parse.token (skipChar ',') |>> (List.map ConnectionOption >> Connection)

        let connectionF =
            function | Connection x -> F.join (fun (ConnectionOption x) -> F.append x) (F.append ",") x

        { Parse = connectionP
          Format = connectionF }

    static member format =
        M.format Connection.Mapping

    static member parse =
        M.parse Connection.Mapping

    static member tryParse =
        M.tryParse Connection.Mapping

    override x.ToString () =
        Connection.format x

and ConnectionOption =
    | ConnectionOption of string

(* RFC 7231

   Types, parsers and formatters implemented to mirror the semantics of
   HTTP as defined in RFC 7231.
   
   See [http://tools.ietf.org/html/rfc7231] *)

(* Media-Type

   Includes the common definition of parameter as defined within this
   section, but applicable to multiple later types.

   Taken from RFC 7231, Section 3.1.1.1 Media-Type
   [http://tools.ietf.org/html/rfc7231#section-3.1.1.1] *)

type MediaType =
    | MediaType of Type * SubType * Parameters

    static member Mapping =

        let mediaTypeP =
            G.Parse.token .>> (skipChar '/') .>>. G.Parse.token .>>. Parameters.Mapping.Parse
            |>> (fun ((x, y), p) -> MediaType (Type x, SubType y, p))

        let mediaTypeF =
            function | MediaType (Type x, SubType y, p) -> 
                        F.appendf2 "{0}/{1}" x y >> Parameters.Mapping.Format p

        { Parse = mediaTypeP
          Format = mediaTypeF }

    (* Optics *)

    static member type_ =
        (fun (MediaType (x, _, _)) -> x),
        (fun x (MediaType (_, y, z)) -> MediaType (x, y, z))

    static member subType_ =
        (fun (MediaType (_, y, _)) -> y),
        (fun y (MediaType (x, _, z)) -> MediaType (x, y, z))

    static member parameters_ =
        (fun (MediaType (_, _, z)) -> z),
        (fun z (MediaType (x, y, _)) -> MediaType (x, y, z))

    (* Common *)

    static member format =
        M.format MediaType.Mapping

    static member parse =
        M.parse MediaType.Mapping

    static member tryParse =
        M.tryParse MediaType.Mapping

    override x.ToString () =
        MediaType.format x

and Parameters =
    | Parameters of Map<string, string>

    static member Mapping =

        let parameterP =
            G.Parse.token .>> skipChar '=' .>>. (G.Parse.quotedString <|> G.Parse.token)

        let parametersP =
            G.Parse.prefix parameterP (skipChar ';') |>> (Map.ofList >> Parameters)

        let pairF =
            (<||) (F.appendf2 "{0}={1}")

        let parametersF =
            function | Parameters (x: Map<string, string>) when Map.isEmpty x -> id
                     | Parameters (x) -> F.append ";" >> F.join pairF (F.append ";") (Map.toList x |> List.rev)

        { Parse = parametersP
          Format = parametersF }

    (* Optics *)

    static member parameters_ =
        (fun (Parameters x) -> x), (Parameters)

and Type =
    | Type of string

and SubType =
    | SubType of string

(* Media-Type Presets *)

type MediaType with

    static member Css =
        MediaType (Type "text", SubType "css", Parameters Map.empty)

    static member Html =
        MediaType (Type "text", SubType "html", Parameters Map.empty)

    static member JavaScript =
        MediaType (Type "application", SubType "javascript", Parameters Map.empty)

    static member Json =
        MediaType (Type "application", SubType "json", Parameters Map.empty)

    /// Convenience definition for "text/plain" without extra parameters
    static member Text =
        MediaType (Type "text", SubType "plain", Parameters Map.empty)

    /// Convenience definition for "application/xml" without extra parameters
    static member Xml =
        MediaType (Type "application", SubType "xml", Parameters Map.empty)

(* Content-Type

    Taken from RFC 7231, Section 3.1.1.5 Content-Type
    [http://tools.ietf.org/html/rfc7231#section-3.1.1.5] *)

type ContentType =
    | ContentType of MediaType

    static member Mapping =

        let contentTypeP =
            MediaType.Mapping.Parse |>> ContentType

        let contentTypeF =
            function | ContentType x -> MediaType.Mapping.Format x

        { Parse = contentTypeP
          Format = contentTypeF }

    (* Optics *)

    static member mediaType_ =
        (fun (ContentType x) -> x), (ContentType)

    (* Common *)

    static member format =
        M.format ContentType.Mapping

    static member parse =
        M.parse ContentType.Mapping

    static member tryParse =
        M.tryParse ContentType.Mapping

    override x.ToString () =
        ContentType.format x

(* Content-Encoding

   Taken from RFC 7231, Section 3.1.2.2 Content-Encoding
   [http://tools.ietf.org/html/rfc7231#section-3.1.2.2] *)

type ContentEncoding =
    | ContentEncoding of ContentCoding list

    static member Mapping =

        let contentEncodingP =
            G.Parse.infix G.Parse.token (skipChar ',') |>> (List.map ContentCoding >> ContentEncoding)

        let contentEncodingF =
            function | ContentEncoding x -> F.join (fun (ContentCoding x) -> F.append x) (F.append ",") x

        { Parse = contentEncodingP
          Format = contentEncodingF }

    static member format =
        M.format ContentEncoding.Mapping

    static member parse =
        M.parse ContentEncoding.Mapping

    static member tryParse =
        M.tryParse ContentEncoding.Mapping

    override x.ToString () =
        ContentEncoding.format x

and ContentCoding =
    | ContentCoding of string

(* Content-Encoding Presets *)

type ContentCoding with

    /// Convenience definition for "compress"
    static member Compress =
        ContentCoding "compress"

    /// Convenience definition for "deflate"
    static member Deflate =
        ContentCoding "deflate"

    /// Convenience definition for "gzip"
    static member GZip =
        ContentCoding "gzip"

(* Content-Language

   Taken from RFC 7231, Section 3.1.3.2 Content-Language
   [http://tools.ietf.org/html/rfc7231#section-3.1.3.2] *)

type ContentLanguage =
    | ContentLanguage of LanguageTag list

    static member Mapping =

        let contentLanguageP =
            G.Parse.infix1 LanguageTag.Mapping.Parse (skipChar ',') |>> ContentLanguage

        let contentLanguageF =
            function | ContentLanguage xs -> F.join LanguageTag.Mapping.Format (F.append ",") xs

        { Parse = contentLanguageP
          Format = contentLanguageF }

    static member format =
        M.format ContentLanguage.Mapping

    static member parse =
        M.parse ContentLanguage.Mapping

    static member tryParse =
        M.tryParse ContentLanguage.Mapping

    override x.ToString () =
        ContentLanguage.format x

(* Content-Location

   Taken from RFC 7231, Section 3.1.4.2 Content-Location
   [http://tools.ietf.org/html/rfc7231#section-3.1.4.2] *)

type ContentLocation =
    | ContentLocation of ContentLocationUri

    static member Mapping =

        let contentLocationP =
            choice [
                attempt AbsoluteUri.Mapping.Parse |>> (Absolute >> ContentLocation)
                PartialUri.Mapping.Parse |>> (Partial >> ContentLocation) ]

        let contentLocationF =
            function | ContentLocation (Absolute x) -> AbsoluteUri.Mapping.Format x
                     | ContentLocation (Partial x) -> PartialUri.Mapping.Format x

        { Parse = contentLocationP
          Format = contentLocationF }

    static member format =
        M.format ContentLocation.Mapping

    static member parse =
        M.parse ContentLocation.Mapping

    static member tryParse =
        M.tryParse ContentLocation.Mapping

    override x.ToString () =
        ContentLocation.format x

and ContentLocationUri =
    | Absolute of AbsoluteUri
    | Partial of PartialUri

(* Method

   Taken from RFC 7231, Section 4
   See [http://tools.ietf.org/html/rfc7231#section-4] *)

type Method =
    | CONNECT
    | DELETE 
    | HEAD 
    | GET 
    | OPTIONS 
    | POST 
    | PUT 
    | TRACE 
    | Custom of string

    static member Mapping =

        let methodP =
            choice [
                skipStringCI "connect" >>% CONNECT
                skipStringCI "delete" >>% DELETE
                skipStringCI "head" >>% HEAD
                skipStringCI "get" >>% GET
                skipStringCI "options" >>% OPTIONS
                skipStringCI "post" >>% POST
                skipStringCI "put" >>% PUT
                skipStringCI "trace" >>% TRACE
                restOfLine false |>> Method.Custom ]

        let methodF =
            function | CONNECT -> F.append "CONNECT"
                     | DELETE -> F.append "DELETE"
                     | HEAD -> F.append "HEAD"
                     | GET -> F.append "GET"
                     | OPTIONS -> F.append "OPTIONS"
                     | POST -> F.append "POST"
                     | PUT -> F.append "PUT"
                     | TRACE -> F.append "TRACE"
                     | Method.Custom x -> F.append x

        { Parse = methodP
          Format = methodF }

    static member format =
        M.format Method.Mapping

    static member parse =
        M.parse Method.Mapping

    override x.ToString () =
        Method.format x

(* Expect

   Taken from RFC 7231, Section 5.1.1 Expect
   See [http://tools.ietf.org/html/rfc7231#section-5.1.1] *)

type Expect =
    | Expect of Continue

    static member Mapping =

        let expectP =
            skipStringCI "100-continue" >>% Expect Continue

        let expectF =
            function | Expect Continue -> F.append "100-continue"

        { Parse = expectP
          Format = expectF }

    static member format =
        M.format Expect.Mapping

    static member parse =
        M.parse Expect.Mapping

    static member tryParse =
        M.tryParse Expect.Mapping

    override x.ToString () =
        Expect.format x

and Continue =
    | Continue

(* Max-Forwards

   Taken from RFC 7231, Section 5.1.2. Max-Forwards
   [http://tools.ietf.org/html/rfc7231#section-5.1.2] *)

type MaxForwards =
    | MaxForwards of int

    static member Mapping =

        let maxForwardsP =
            puint32 |>> (int >> MaxForwards)

        let maxForwardsF =
            function | MaxForwards x -> F.append (string x)

        { Parse = maxForwardsP
          Format = maxForwardsF }

    static member format =
        M.format MaxForwards.Mapping

    static member parse =
        M.parse MaxForwards.Mapping

    static member tryParse =
        M.tryParse MaxForwards.Mapping

    override x.ToString () =
        MaxForwards.format x

(* Quality Values

   Taken from RFC 7231, Section 5.3.1. Quality Values
   [http://tools.ietf.org/html/rfc7231#section-5.3.1] *)

type Weight =
    | Weight of float

    static member Mapping =

        let valueOrDefault =
            function | Some x -> float (sprintf "0.%s" x)
                     | _ -> 0.

        let d3P =
                manyMinMaxSatisfy 0 3 (int >> G.isDigit) 
            .>> notFollowedBy (skipSatisfy (int >> G.isDigit))

        let d03P =
                skipManyMinMaxSatisfy 0 3 ((=) '0') 
            .>> notFollowedBy (skipSatisfy (int >> G.isDigit))

        let qvalueP =
            choice [ 
                skipChar '0' >>. opt (skipChar '.' >>. d3P) |>> valueOrDefault
                skipChar '1' >>. optional (skipChar '.' >>. d03P) >>% 1. ]

        let weightP =
            (skipChar ';') >>. G.Parse.ows >>. skipStringCI "q=" >>. qvalueP .>> G.Parse.ows |>> Weight

        let weightF =
            function | Weight x -> F.appendf1 ";q={0:G4}" x

        { Parse = weightP
          Format = weightF }

(* Accept

   Taken from RFC 7231, Section 5.3.2. Accept
   [http://tools.ietf.org/html/rfc7231#section-5.3.2] *)

type Accept =
    | Accept of AcceptableMedia list

    static member Mapping =

        let acceptP =
            G.Parse.infix AcceptableMedia.Mapping.Parse (skipChar ',') |>> Accept

        let acceptF =
            function | Accept x -> F.join AcceptableMedia.Mapping.Format (F.append ",") x

        { Parse = acceptP
          Format = acceptF }

    static member format =
        M.format Accept.Mapping

    static member parse =
        M.parse Accept.Mapping

    static member tryParse =
        M.tryParse Accept.Mapping

    override x.ToString () =
        Accept.format x

and AcceptableMedia =
    | AcceptableMedia of MediaRange * AcceptParameters option

    static member Mapping =

        let acceptableMediaP = 
            MediaRange.Mapping.Parse .>>. opt AcceptParameters.Mapping.Parse
            |>> AcceptableMedia

        let acceptableMediaF =
            function | AcceptableMedia (m, p) -> 
                         MediaRange.Mapping.Format m 
                         >> (function | Some p -> AcceptParameters.Mapping.Format p
                                      | _ -> id) p

        { Parse = acceptableMediaP
          Format = acceptableMediaF }

and MediaRange =
    | Closed of Type * SubType * Parameters
    | Partial of Type * Parameters
    | Open of Parameters

    static member Mapping =

        let mediaRangeParameterP =
            notFollowedBy (G.Parse.ows >>. skipStringCI "q=") >>. G.Parse.token .>> skipChar '=' .>>. G.Parse.token

        let mediaRangeParametersP =
            G.Parse.prefix mediaRangeParameterP (skipChar ';') |>> Map.ofList

        let openMediaRangeP = 
            skipString "*/*" >>. G.Parse.ows >>. mediaRangeParametersP |>> (Parameters >> MediaRange.Open)

        let partialMediaRangeP = 
            G.Parse.token .>> skipString "/*" .>> G.Parse.ows .>>. mediaRangeParametersP
            |>> fun (x, parameters) -> 
                    MediaRange.Partial (Type x, Parameters parameters)

        let closedMediaRangeP = 
            G.Parse.token .>> skipChar '/' .>>. G.Parse.token .>> G.Parse.ows .>>. mediaRangeParametersP
            |>> fun ((x, y), parameters) -> 
                    MediaRange.Closed (Type x, SubType y, Parameters parameters)

        let mediaRangeP = 
            choice [
                attempt openMediaRangeP
                attempt partialMediaRangeP
                closedMediaRangeP ]

        let mediaRangeF =
            function | MediaRange.Closed (Type x, SubType y, p) -> F.appendf2 "{0}/{1}" x y >> Parameters.Mapping.Format p
                     | MediaRange.Partial (Type x, p) -> F.appendf1 "{0}/*" x >> Parameters.Mapping.Format p
                     | MediaRange.Open p -> F.append "*/*" >> Parameters.Mapping.Format p

        { Parse = mediaRangeP
          Format = mediaRangeF }

and AcceptParameters =
    | AcceptParameters of Weight * AcceptExtensions

    static member Mapping =

        let acceptParamsP =
            Weight.Mapping.Parse .>> G.Parse.ows .>>. AcceptExtensions.Mapping.Parse
            |>> AcceptParameters

        let acceptParamsF =
            function | AcceptParameters (w, e) -> 
                        Weight.Mapping.Format w >> AcceptExtensions.Mapping.Format e

        { Parse = acceptParamsP
          Format = acceptParamsF }

and AcceptExtensions =
    | Extensions of Map<string, string option>

    static member Mapping =

        let acceptExtP =
            G.Parse.token .>>. opt (skipChar '=' >>. (G.Parse.quotedString <|> G.Parse.token))

        let acceptExtsP =
            G.Parse.prefix acceptExtP (skipChar ';') |>> (Map.ofList >> Extensions)

        let acceptExtsF =
            function | Extensions (x: Map<string, string option>) when Map.isEmpty x -> id
                     | _ -> id

        { Parse = acceptExtsP
          Format = acceptExtsF }

(* Accept-Charset

   Taken from RFC 7231, Section 5.3.3 Accept-Charset
   [http://tools.ietf.org/html/rfc7231#section-5.3.3] *)

type AcceptCharset =
    | AcceptCharset of AcceptableCharset list

    static member Mapping =

        let acceptCharsetP =
            G.Parse.infix1 AcceptableCharset.Mapping.Parse (skipChar ',')
            |>> AcceptCharset

        let acceptCharsetF =
            function | AcceptCharset x ->
                        F.join AcceptableCharset.Mapping.Format (F.append ",") x

        { Parse = acceptCharsetP
          Format = acceptCharsetF }

    static member format =
        M.format AcceptCharset.Mapping

    static member parse =
        M.parse AcceptCharset.Mapping

    static member tryParse =
        M.tryParse AcceptCharset.Mapping

    override x.ToString () =
        AcceptCharset.format x

and AcceptableCharset =
    | AcceptableCharset of CharsetRange * Weight option

    static member Mapping =

        let acceptableCharsetP =
            CharsetRange.Mapping.Parse .>> G.Parse.ows .>>. opt Weight.Mapping.Parse
            |>> AcceptableCharset

        let acceptableCharsetF =
            function | AcceptableCharset (c, w) ->
                        CharsetRange.Mapping.Format c 
                        >> (function | Some w -> Weight.Mapping.Format w
                                     | _ -> id) w

        { Parse = acceptableCharsetP
          Format = acceptableCharsetF }

and CharsetRange =
    | Charset of Charset
    | Any

    static member Mapping =

        let charsetRangeAnyP =
            skipChar '*' >>% CharsetRange.Any

        let charsetRangeCharsetP =
            G.Parse.token |>> (Charset.Charset >> Charset)

        let charsetRangeP = 
            choice [
                attempt charsetRangeAnyP
                charsetRangeCharsetP ]

        let charsetRangeF =
            function | Charset (Charset.Charset x) -> F.append x
                     | Any -> F.append "*"

        { Parse = charsetRangeP
          Format = charsetRangeF }

and Charset =
    | Charset of string

(* Charset Presets *)

type Charset with

    /// Convenience definition for "iso-8859-1"
    static member Iso88591 =
        Charset "iso-8859-1"

    /// Convenience definition for "unicode-1-1"
    static member Unicode =
        Charset "unicode-1-1"

    /// Convenience definition for "utf-8"
    static member Utf8 =
        Charset "utf-8"

(* Accept-Encoding

   Taken from RFC 7231, Section 5.3.4. Accept-Encoding
   [http://tools.ietf.org/html/rfc7231#section-5.3.4] *)

type AcceptEncoding =
    | AcceptEncoding of AcceptableEncoding list

    static member Mapping =

        let acceptEncodingP =
            G.Parse.infix AcceptableEncoding.Mapping.Parse (skipChar ',')
            |>> AcceptEncoding

        let acceptEncodingF =
            function | AcceptEncoding x ->
                        F.join AcceptableEncoding.Mapping.Format (F.append ",") x

        { Parse = acceptEncodingP
          Format = acceptEncodingF }

    static member format =
        M.format AcceptEncoding.Mapping

    static member parse =
        M.parse AcceptEncoding.Mapping

    static member tryParse =
        M.tryParse AcceptEncoding.Mapping

    override x.ToString () =
        AcceptEncoding.format x

and AcceptableEncoding =
    | AcceptableEncoding of EncodingRange * Weight option

    static member Mapping =

        let acceptableEncodingP =
            EncodingRange.Mapping.Parse .>> G.Parse.ows .>>. opt Weight.Mapping.Parse
            |>> AcceptableEncoding

        let acceptableEncodingF =
            function | AcceptableEncoding (e, w) ->
                        EncodingRange.Mapping.Format e
                        >> (function | Some w -> Weight.Mapping.Format w
                                     | _ -> id) w

        { Parse = acceptableEncodingP
          Format = acceptableEncodingF }

and EncodingRange =
    | Coding of ContentCoding
    | Identity
    | Any

    static member Mapping =

        let encodingRangeAnyP =
            skipChar '*' >>% Any

        let encodingRangeIdentityP =
            skipStringCI "identity" >>% Identity

        let encodingRangeCodingP =
            G.Parse.token |>> (ContentCoding >> Coding)

        let encodingRangeP =
            choice [
                attempt encodingRangeAnyP
                attempt encodingRangeIdentityP
                encodingRangeCodingP ]

        let encodingRangeF =
            function | Coding (ContentCoding x) -> F.append x
                     | Identity -> F.append "identity"
                     | Any -> F.append "*"

        { Parse = encodingRangeP
          Format = encodingRangeF }

(* Accept-Language

   Taken from RFC 7231, Section 5.3.5. Accept-Language
   [http://tools.ietf.org/html/rfc7231#section-5.3.5] *)

type AcceptLanguage =
    | AcceptLanguage of AcceptableLanguage list

    static member Mapping =

        let acceptLanguageP =
            G.Parse.infix AcceptableLanguage.Mapping.Parse (skipChar ',')
            |>> AcceptLanguage

        let acceptLanguageF =
            function | AcceptLanguage x ->
                        F.join AcceptableLanguage.Mapping.Format (F.append ",") x

        { Parse = acceptLanguageP
          Format = acceptLanguageF }

    static member format =
        M.format AcceptLanguage.Mapping

    static member parse =
        M.parse AcceptLanguage.Mapping

    static member tryParse =
        M.tryParse AcceptLanguage.Mapping

    override x.ToString () =
        AcceptLanguage.format x

and AcceptableLanguage =
    | AcceptableLanguage of LanguageRange * Weight option

    static member Mapping =

        let acceptableLanguageP =
            LanguageRange.Mapping.Parse .>> G.Parse.ows .>>. opt Weight.Mapping.Parse
            |>> AcceptableLanguage

        let acceptableLanguageF =
            function | AcceptableLanguage (l, w) ->
                        LanguageRange.Mapping.Format l
                        >> (function | Some w -> Weight.Mapping.Format w
                                     | _ -> id) w

        { Parse = acceptableLanguageP
          Format = acceptableLanguageF }

(* Referer

   Taken from RFC 7231, Section 5.5.2 Referer
   [http://tools.ietf.org/html/rfc7231#section-5.5.2] *)

type Referer =
    | Referer of RefererUri

    static member Mapping =

        let refererP =
            choice [
                attempt AbsoluteUri.Mapping.Parse |>> (Absolute >> Referer)
                PartialUri.Mapping.Parse |>> (Partial >> Referer) ]

        let refererF =
            function | Referer (Absolute x) -> AbsoluteUri.Mapping.Format x
                     | Referer (Partial x) -> PartialUri.Mapping.Format x

        { Parse = refererP
          Format = refererF }

    static member format =
        M.format Referer.Mapping

    static member parse =
        M.parse Referer.Mapping

    static member tryParse =
        M.tryParse Referer.Mapping

    override x.ToString () =
        Referer.format x
    
and RefererUri =
    | Absolute of AbsoluteUri
    | Partial of PartialUri

(* HTTP-Date

   Taken from RFC 7231, Section 7.1.1.1 HTTP-Date *)

[<RequireQualifiedAccess>]
module HttpDate =

    let private dateTimeFormat =
        CultureInfo.InvariantCulture.DateTimeFormat

    let private dateTimeAdjustment =
        DateTimeStyles.AdjustToUniversal

    [<RequireQualifiedAccess>]
    module Parse =

        let httpDate p : Parser<DateTime, unit> =
            p >>= (fun s ->
                match DateTime.TryParse (s, dateTimeFormat, dateTimeAdjustment) with
                | true, d -> preturn d
                | _ -> pzero)

(* Date

   Taken from RFC 7231, Section 7.1.1.2 Date
   [http://tools.ietf.org/html/rfc7231#section-7.1.1.2] *)

type Date =
    | Date of DateTime

    static member Mapping =

        let dateP =
            HttpDate.Parse.httpDate (restOfLine false) |>> Date.Date

        let dateF =
            function | Date.Date x -> F.append (x.ToString "r")

        { Parse = dateP
          Format = dateF }

    static member format =
        M.format Date.Mapping

    static member parse =
        M.parse Date.Mapping

    static member tryParse =
        M.tryParse Date.Mapping

    override x.ToString () =
        Date.format x

(* Location

   Taken from RFC 7231, Section 7.1.2 Location
   [http://tools.ietf.org/html/rfc7231#section-7.1.2] *)

type Location =
    | Location of UriReference

    static member Mapping =

        let locationP =
            UriReference.Mapping.Parse |>> Location

        let locationF =
            function | Location x -> UriReference.Mapping.Format x

        { Parse = locationP
          Format = locationF }

    static member format =
        M.format Location.Mapping

    static member parse =
        M.parse Location.Mapping

    static member tryParse =
        M.tryParse Location.Mapping

    override x.ToString () =
        Location.format x

(* Retry-After

   Taken from RFC 7231, Section 7.1.3. Retry-After
   [http://tools.ietf.org/html/rfc7231#section-7.1.3] *)

type RetryAfter =
    | RetryAfter of RetryAfterChoice

    static member Mapping =

        let retryAfterP =
            choice [
                attempt (HttpDate.Parse.httpDate (restOfLine false)) |>> (Date >> RetryAfter)
                puint32 |>> (float >> TimeSpan.FromSeconds >> Delay >> RetryAfter) ]

        let retryAfterF =
            function | RetryAfter (Date x) -> F.append (x.ToString "r")
                     | RetryAfter (Delay x) -> F.append (string (int x.TotalSeconds))

        { Parse = retryAfterP
          Format = retryAfterF }

    static member format =
        M.format RetryAfter.Mapping

    static member parse =
        M.parse RetryAfter.Mapping

    static member tryParse =
        M.tryParse RetryAfter.Mapping

    override x.ToString () =
        RetryAfter.format x

and RetryAfterChoice =
    | Date of DateTime
    | Delay of TimeSpan

(* Allow

   Taken from RFC 7231, Section 7.4.1 Allow
   [http://tools.ietf.org/html/rfc7231#section-7.4.1] *)

type Allow =
    | Allow of Method list

    static member Mapping =

        let allowP =
            G.Parse.infix Method.Mapping.Parse (skipChar ',') |>> Allow

        let allowF =
            function | Allow x -> F.join Method.Mapping.Format (F.append ",") x

        { Parse = allowP
          Format = allowF }

    static member format =
        M.format Allow.Mapping

    static member parse =
        M.parse Allow.Mapping

    static member tryParse =
        M.tryParse Allow.Mapping

    override x.ToString () =
        Allow.format x

(* RFC 7232

   Types, parsers and formatters implemented to mirror the semantics of
   HTTP as defined in RFC 7232.

   See [http://tools.ietf.org/html/rfc7232] *)

(* Last-Modified

   Taken from RFC 7232, Section 2.2 Last-Modified
   [http://tools.ietf.org/html/rfc7232#section-2.2] *)

type LastModified =
    | LastModified of DateTime

    static member Mapping =

        let lastModifiedP =
            HttpDate.Parse.httpDate (restOfLine false) |>> LastModified

        let lastModifiedF =
            function | LastModified x -> F.append (x.ToString "r")

        { Parse = lastModifiedP
          Format = lastModifiedF }

    static member format =
        M.format LastModified.Mapping

    static member parse =
        M.parse LastModified.Mapping

    static member tryParse =
        M.tryParse LastModified.Mapping

    override x.ToString () =
        LastModified.format x

(* ETag

   Taken from RFC 7232 Section 2.3 ETag
   [http://tools.ietf.org/html/rfc7232#section-2.3] *)

type ETag =
    | ETag of EntityTag

    static member Mapping =

        let eTagP =
            EntityTag.Mapping.Parse |>> ETag

        let eTagF =
            function | ETag x -> EntityTag.Mapping.Format x

        { Parse = eTagP
          Format = eTagF }

    static member format =
        M.format ETag.Mapping

    static member parse =
        M.parse ETag.Mapping

    static member tryParse =
        M.tryParse ETag.Mapping

    override x.ToString () =
        ETag.format x

and EntityTag =
    | Strong of string
    | Weak of string

    static member Mapping =

        let eTagChar i =
                i = 0x21
             || i >= 0x23 && i <= 0x7e
             || G.isObstext i

        let opaqueTagP =
                skipSatisfy (int >> G.isDquote) 
            >>. manySatisfy (int >> eTagChar) 
            .>> skipSatisfy (int >> G.isDquote)

        let entityTagP =
            choice [
                attempt (skipString "W/" >>. opaqueTagP |>> Weak)
                opaqueTagP |>> Strong ]

        let entityTagF =
            function | Strong x -> F.appendf1 "\"{0}\"" x
                     | Weak x -> F.appendf1 "W/\"{0}\"" x

        { Parse = entityTagP
          Format = entityTagF }

(* If-Match

   Taken from RFC 7232, Section 3.1, If-Match
   [http://tools.ietf.org/html/rfc7232#section-3.1] *)

type IfMatch =
    | IfMatch of IfMatchChoice

    static member Mapping =

        let ifMatchP =
            choice [
                skipChar '*' >>% IfMatch (Any)
                G.Parse.infix EntityTag.Mapping.Parse (skipChar ',') |>> (EntityTags >> IfMatch) ]

        let ifMatchF =
            function | IfMatch (EntityTags x) -> F.join EntityTag.Mapping.Format (F.append ",") x
                     | IfMatch (Any) -> F.append "*"

        { Parse = ifMatchP
          Format = ifMatchF }

    static member format =
        M.format IfMatch.Mapping

    static member parse =
        M.parse IfMatch.Mapping

    static member tryParse =
        M.tryParse IfMatch.Mapping

    override x.ToString () =
        IfMatch.format x

and IfMatchChoice =
    | EntityTags of EntityTag list
    | Any

(* If-None-Match

   Taken from RFC 7232, Section 3.2, If-None-Match
   [http://tools.ietf.org/html/rfc7232#section-3.2] *)

type IfNoneMatch =
    | IfNoneMatch of IfNoneMatchChoice

    static member Mapping =

        let ifNoneMatchP =
            choice [
                skipChar '*' >>% IfNoneMatch (Any)
                G.Parse.infix EntityTag.Mapping.Parse (skipChar ',') |>> (EntityTags >> IfNoneMatch) ]

        let ifNoneMatchF =
            function | IfNoneMatch (EntityTags x) -> F.join EntityTag.Mapping.Format (F.append ",") x
                     | IfNoneMatch (Any) -> F.append "*"

        { Parse = ifNoneMatchP
          Format = ifNoneMatchF }

    static member format =
        M.format IfNoneMatch.Mapping

    static member parse =
        M.parse IfNoneMatch.Mapping

    static member tryParse =
        M.tryParse IfNoneMatch.Mapping

    override x.ToString () =
        IfNoneMatch.format x

and IfNoneMatchChoice =
    | EntityTags of EntityTag list
    | Any

(* If-Modified-Since

   Taken from RFC 7232, Section 3.3, If-Modified-Since
   [http://tools.ietf.org/html/rfc7232#section-3.3] *)

type IfModifiedSince =
    | IfModifiedSince of DateTime

    static member Mapping =

        let ifModifiedSinceP =
            HttpDate.Parse.httpDate (restOfLine false) |>> IfModifiedSince

        let ifModifiedSinceF =
            function | IfModifiedSince x -> F.append (x.ToString "r")

        { Parse = ifModifiedSinceP
          Format = ifModifiedSinceF }

    static member format =
        M.format IfModifiedSince.Mapping

    static member parse =
        M.parse IfModifiedSince.Mapping

    static member tryParse =
        M.tryParse IfModifiedSince.Mapping

    override x.ToString () =
        IfModifiedSince.format x

(* If-Unmodified-Since

   Taken from RFC 7232, Section 3.4, If-Unmodified-Since
   [http://tools.ietf.org/html/rfc7232#section-3.4] *)

type IfUnmodifiedSince =
    | IfUnmodifiedSince of DateTime

    static member Mapping =

        let ifUnmodifiedSinceP =
            HttpDate.Parse.httpDate (restOfLine false) |>> IfUnmodifiedSince

        let ifUnmodifiedSinceF =
            function | IfUnmodifiedSince x -> F.append (x.ToString "r")

        { Parse = ifUnmodifiedSinceP
          Format = ifUnmodifiedSinceF }

    static member format =
        M.format IfUnmodifiedSince.Mapping

    static member parse =
        M.parse IfUnmodifiedSince.Mapping

    static member tryParse =
        M.tryParse IfUnmodifiedSince.Mapping

    override x.ToString () =
        IfUnmodifiedSince.format x

(* RFC 7233

   Types, parsers and formatters implemented to mirror the semantics of
   HTTP as defined in RFC 7233.
   
   See [http://tools.ietf.org/html/rfc7233] *)

(* If-Range

   Taken from RFC 7233, Section 3.2 If-Range
   See [http://tools.ietf.org/html/rfc7233#section-3.2] *)

type IfRange =
    | IfRange of IfRangeChoice

    static member Mapping =

        let ifRangeP =
            (EntityTag.Mapping.Parse |>> (EntityTag >> IfRange)) <|> 
                                         (HttpDate.Parse.httpDate (restOfLine false) |>> (Date >> IfRange))

        let ifRangeF =
            function | IfRange (Date x) -> F.append (x.ToString "r")
                     | IfRange (EntityTag x) -> EntityTag.Mapping.Format x
        
        { Parse = ifRangeP
          Format = ifRangeF }

    static member format =
        M.format IfRange.Mapping

    static member parse =
        M.parse IfRange.Mapping

    static member tryParse =
        M.tryParse IfRange.Mapping

    override x.ToString () =
        IfRange.format x

and IfRangeChoice =
    | Date of DateTime
    | EntityTag of EntityTag

(* RFC 7234

   Types, parsers and formatters implemented to mirror the semantics of
   HTTP as defined in RFC 7234.

   See [http://tools.ietf.org/html/rfc7234] *)

(* Age

   Taken from RFC 7234 Section 5.1 Age
   See [http://tools.ietf.org/html/rfc7234#section-5.1] *)

type Age =
    | Age of TimeSpan

    static member Mapping =

        let ageP =
            puint32 |>> (float >> TimeSpan.FromSeconds >> Age)

        let ageF =
            function | Age x -> F.append (string x.TotalSeconds)

        { Parse = ageP
          Format = ageF }

    static member format =
        M.format Age.Mapping

    static member parse =
        M.parse Age.Mapping

    static member tryParse =
        M.tryParse Age.Mapping

    override x.ToString () =
        Age.format x

(* Cache-Control

   Taken from RFC 7234 Section 5.2 Cache-Control
   See [http://tools.ietf.org/html/rfc7234#section-5.2]

   Note that from a type system perspective we don't currently
   distinguish between cache-directives that are valid for
   requests/responses or both. This may be worth changing in future,
   but for now it should hopefully be clear enough when used. *)

type CacheControl =
    | CacheControl of CacheDirective list

    static member Mapping =

        let cacheControlP =
            G.Parse.infix1 CacheDirective.Mapping.Parse (skipChar ',') |>> CacheControl

        let cacheControlF =
            function | CacheControl x -> F.join CacheDirective.Mapping.Format (F.append ",") x

        { Parse = cacheControlP
          Format = cacheControlF }

    static member format =
        M.format CacheControl.Mapping

    static member parse =
        M.parse CacheControl.Mapping

    static member tryParse =
        M.tryParse CacheControl.Mapping

    override x.ToString () =
        CacheControl.format x

and CacheDirective =
    | MaxAge of TimeSpan
    | MaxStale of TimeSpan
    | MinFresh of TimeSpan
    | MustRevalidate
    | NoCache
    | NoStore
    | NoTransform
    | OnlyIfCached
    | Private
    | ProxyRevalidate
    | Public
    | SMaxAge of TimeSpan
    | Custom of string * string option

    static member Mapping =

        // TODO: Custom Directive Parsing

        let cacheDirectiveP =
            choice [
                attempt (skipStringCI "max-age=" >>. puint32 |>> (float >> TimeSpan.FromSeconds >> MaxAge))
                attempt (skipStringCI "max-stale=" >>. puint32 |>> (float >> TimeSpan.FromSeconds >> MaxStale))
                attempt (skipStringCI "min-fresh=" >>. puint32 |>> (float >> TimeSpan.FromSeconds >> MinFresh))
                attempt (skipStringCI "must-revalidate" >>% MustRevalidate)
                attempt (skipStringCI "no-cache" >>% NoCache)
                attempt (skipStringCI "no-store" >>% NoStore)
                attempt (skipStringCI "no-transform" >>% NoTransform)
                attempt (skipStringCI "only-if-cached" >>% OnlyIfCached)
                attempt (skipStringCI "private" >>% Private)
                attempt (skipStringCI "proxy-revalidate" >>% ProxyRevalidate)
                attempt (skipStringCI "public" >>% Public)
                attempt (skipStringCI "s-maxage=" >>. puint32 |>> (float >> TimeSpan.FromSeconds >> SMaxAge)) ] 

        let cacheDirectiveF =
            function | MaxAge x -> F.appendf1 "max-age={0}" (int x.TotalSeconds)
                     | MaxStale x -> F.appendf1 "max-stale={0}" (int x.TotalSeconds)
                     | MinFresh x -> F.appendf1 "min-fresh={0}" (int x.TotalSeconds)
                     | MustRevalidate -> F.append "must-revalidate"
                     | NoCache -> F.append "no-cache"
                     | NoStore -> F.append "no-store"
                     | NoTransform -> F.append "no-transform"
                     | OnlyIfCached -> F.append "only-if-cached"
                     | Private -> F.append "private"
                     | ProxyRevalidate -> F.append "proxy-revalidate"
                     | Public -> F.append "public"
                     | SMaxAge x -> F.appendf1 "s-maxage={0}" x
                     | Custom (x, Some y) -> F.appendf2 "{0}={2}" x y
                     | Custom (x, _) -> F.append x

        { Parse = cacheDirectiveP
          Format = cacheDirectiveF }

(* Expires

   Taken from RFC 7234 Section 5.3 Expires
   See [http://tools.ietf.org/html/rfc7234#section-5.3] *)

type Expires =
    | Expires of DateTime

    static member Mapping =

        let expiresP =
            HttpDate.Parse.httpDate (restOfLine false) |>> Expires

        let expiresF =
            function | Expires x -> F.append (x.ToString "r")

        { Parse = expiresP
          Format = expiresF }

    static member format =
        M.format Expires.Mapping

    static member parse =
        M.parse Expires.Mapping

    static member tryParse =
        M.tryParse Expires.Mapping

    override x.ToString () =
        Expires.format x