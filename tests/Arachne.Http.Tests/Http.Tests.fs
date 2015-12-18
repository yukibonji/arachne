module Arachne.Http.Tests

open System
open Arachne.Core.Tests
open Arachne.Http
open Arachne.Language
open Arachne.Uri
open Xunit

(* RFC 7230 *)

[<Fact>]
let ``PartialUri Formatting/Parsing`` () =

    let partialUriTyped : PartialUri =
        PartialUri (
            Absolute (PathAbsolute [ "some"; "path" ]),
            Some (Query "key=val"))

    let partialUriString =
        "/some/path?key=val"

    roundTrip (PartialUri.Format, PartialUri.Parse) [
        partialUriTyped, partialUriString ]

[<Fact>]
let ``HttpVersion Formatting/Parsing`` () =
    
    let httpVersionTyped = HTTP 1.1
    let httpVersionString = "HTTP/1.1"

    roundTrip (HttpVersion.Format, HttpVersion.Parse) [
        httpVersionTyped, httpVersionString ]

[<Fact>]
let ``ContentLength Formatting/Parsing`` () =
    
    let contentLengthTyped = ContentLength 1024
    let contentLengthString = "1024"

    roundTrip (ContentLength.Format, ContentLength.Parse) [
        contentLengthTyped, contentLengthString ]

[<Fact>]
let ``Host Formatting/Parsing`` () =

    let hostTyped = Host (Name (RegName "www.example.com"), Some (Port 8080))
    let hostString = "www.example.com:8080"

    roundTrip (Host.Format, Host.Parse) [
        hostTyped, hostString ]

[<Fact>]
let ``Connection Formatting/Parsing`` () =

    let connectionTyped = Connection ([ ConnectionOption "close"; ConnectionOption "test" ])
    let connectionString = "close,test"

    roundTrip (Connection.Format, Connection.Parse) [
        connectionTyped, connectionString ]

(* RFC 7231 *)

[<Fact>]
let ``MediaType Formatting/Parsing`` () =

    let mediaTypeTyped =
        MediaType (
            Type "application",
            SubType "json", 
            Parameters (Map.ofList [ "charset", "utf-8" ]))

    let mediaTypeString =
        "application/json;charset=utf-8"

    roundTrip (MediaType.Format, MediaType.Parse) [
        mediaTypeTyped, mediaTypeString ]

[<Fact>]
let ``ContentType Formatting/Parsing`` () =

    let contentTypeTyped =
        ContentType (
            MediaType (
                Type "application",
                SubType "json",
                Parameters (Map.ofList [ "charset", "utf-8" ])))

    let contentTypeString =
        "application/json;charset=utf-8"

    roundTrip (ContentType.Format, ContentType.Parse) [
        contentTypeTyped, contentTypeString ]

[<Fact>]
let ``ContentEncoding Formatting/Parsing`` () =

    let contentEncodingTyped =
        ContentEncoding [ 
            ContentCoding "compress"
            ContentCoding "deflate" ]

    let contentEncodingString =
        "compress,deflate"

    roundTrip (ContentEncoding.Format, ContentEncoding.Parse) [
        contentEncodingTyped, contentEncodingString ]

[<Fact>]
let ``ContentLanguage Formatting/Parsing`` () =

    let contentLanguageTyped =
        ContentLanguage [
            LanguageTag (
                Language ("en", None),
                None,
                Some (Region "GB"),
                Variant [])
            LanguageTag (
                Language ("hy", None),
                Some (Script "Latn"),
                Some (Region "IT"),
                Variant [ "arvela" ]) ]

    let contentLanguageString =
        "en-GB,hy-Latn-IT-arvela"

    roundTrip (ContentLanguage.Format, ContentLanguage.Parse) [
        contentLanguageTyped, contentLanguageString ]

[<Fact>]
let ``ContentLocation Formatting/Parsing`` () =
    
    let contentLocationTyped =
        ContentLocation (
            ContentLocationUri.Absolute (
                AbsoluteUri (
                    Scheme "http",
                    HierarchyPart.Absolute (PathAbsolute [ "some"; "path" ]),
                    None)))

    let contentLocationString =
        "http:/some/path"

    roundTrip (ContentLocation.Format, ContentLocation.Parse) [
        contentLocationTyped, contentLocationString ]

[<Fact>]
let ``Method Formatting/Parsing`` () =

    roundTrip (Method.Format, Method.Parse) [
        Method.GET, "GET"
        Method.Custom "PATCH", "PATCH" ]

[<Fact>]
let ``Expect Formatting/Parsing`` () =

    roundTrip (Expect.Format, Expect.Parse) [
        Expect (Continue), "100-continue" ]

[<Fact>]
let ``MaxForwards Formatting/Parsing`` () =
    
    roundTrip (MaxForwards.Format, MaxForwards.Parse) [
        MaxForwards 10, "10" ]

[<Fact>]
let ``Accept Formatting/Parsing`` () =

    let acceptTyped =
        Accept [
            AcceptableMedia (
                Closed (Type "application", SubType "json", Parameters Map.empty),
                Some (AcceptParameters (Weight 0.7, Extensions Map.empty)))
            AcceptableMedia (
                MediaRange.Partial (Type "text", Parameters Map.empty),
                Some (AcceptParameters (Weight 0.5, Extensions Map.empty)))
            AcceptableMedia (
                Open (Parameters Map.empty),
                None) ]

    let acceptString =
        "application/json;q=0.7,text/*;q=0.5,*/*"

    roundTrip (Accept.Format, Accept.Parse) [
        acceptTyped, acceptString ]

[<Fact>]
let ``AcceptCharset Formatting/Parsing`` () =
    
    let acceptCharsetTyped =
        AcceptCharset [
            AcceptableCharset (
                CharsetRange.Charset (Charset.Utf8),
                Some (Weight 0.7))
            AcceptableCharset (
                CharsetRange.Any,
                Some (Weight 0.2)) ]

    let acceptCharsetString =
        "utf-8;q=0.7,*;q=0.2"

    roundTrip (AcceptCharset.Format, AcceptCharset.Parse) [
        acceptCharsetTyped, acceptCharsetString ]

[<Fact>]
let ``AcceptEncoding Formatting/Parsing`` () =

    let acceptEncodingTyped =
        AcceptEncoding [
            AcceptableEncoding (
                EncodingRange.Coding (ContentCoding.Compress),
                Some (Weight 0.8))
            AcceptableEncoding (
                EncodingRange.Identity,
                None)
            AcceptableEncoding (
                EncodingRange.Any,
                Some (Weight 0.3)) ]

    let acceptEncodingString =
        "compress;q=0.8,identity,*;q=0.3"

    roundTrip (AcceptEncoding.Format, AcceptEncoding.Parse) [
        acceptEncodingTyped, acceptEncodingString ]

[<Fact>]
let ``AcceptLanguage Formatting/Parsing`` () =

    let acceptLanguageTyped =
        AcceptLanguage [
            AcceptableLanguage (
                Range [ "en"; "GB" ],
                Some (Weight 0.8))
            AcceptableLanguage (
                Any,
                None) ]

    let acceptLanguageString =
        "en-GB;q=0.8,*"

    roundTrip (AcceptLanguage.Format, AcceptLanguage.Parse) [
        acceptLanguageTyped, acceptLanguageString ]

[<Fact>]
let ``Referer Formatting/Parsing`` () =

    let refererTyped =
        Referer (
            Partial (
                PartialUri (
                    RelativePart.Absolute (PathAbsolute ["some"; "path" ]),
                    None)))

    let refererString =
        "/some/path"

    roundTrip (Referer.Format, Referer.Parse) [
        refererTyped, refererString ]

[<Fact>]
let ``Date Formatting/Parsing`` () =

    let dateTyped =
        Date.Date (DateTime.Parse ("1994/10/29 19:43:31"))

    let dateString =
        "Sat, 29 Oct 1994 19:43:31 GMT"

    roundTrip (Date.Format, Date.Parse) [
        dateTyped, dateString ]

[<Fact>]
let ``Location Formatting/Parsing`` () =

    let locationTyped =
        Location (
            UriReference.Uri (
                Uri.Uri (
                    Scheme "http",
                    HierarchyPart.Absolute (PathAbsolute [ "some"; "path" ]),
                    None,
                    None)))

    let locationString =
        "http:/some/path"

    roundTrip (Location.Format, Location.Parse) [
        locationTyped, locationString ]

[<Fact>]
let ``RetryAfter Formatting/Parsing`` () =

    let retryAfterTyped =
        RetryAfter (Delay (TimeSpan.FromSeconds (float 60)))

    let retryAfterString =
        "60"

    roundTrip (RetryAfter.Format, RetryAfter.Parse) [
        retryAfterTyped, retryAfterString ]

[<Fact>]
let ``Allow Formatting/Parsing`` () =

    let allowTyped =
        Allow [ DELETE; GET; POST; PUT ]

    let allowString =
        "DELETE,GET,POST,PUT"

    roundTrip (Allow.Format, Allow.Parse) [
        allowTyped, allowString ]

(* RFC 7232 *)

[<Fact>]
let ``LastModified Formatting/Parsing`` () =

    let lastModifiedTyped =
        LastModified (DateTime.Parse ("1994/10/29 19:43:31"))

    let lastModifiedString =
        "Sat, 29 Oct 1994 19:43:31 GMT"

    roundTrip (LastModified.Format, LastModified.Parse) [
        lastModifiedTyped, lastModifiedString ]

[<Fact>]
let ``ETag Formatting/Parsing`` () =

    let eTagTyped =
        ETag (Strong "sometag")

    let eTagString =
        "\"sometag\""

    roundTrip (ETag.Format, ETag.Parse) [
        eTagTyped, eTagString ]

[<Fact>]
let ``IfMatch Formatting/Parsing`` () =

    let ifMatchTyped =
        IfMatch (IfMatchChoice.EntityTags [ Strong "sometag"; Weak "othertag" ])

    let ifMatchString =
        "\"sometag\",W/\"othertag\""

    roundTrip (IfMatch.Format, IfMatch.Parse) [
        ifMatchTyped, ifMatchString ]

[<Fact>]
let ``IfNoneMatch Formatting/Parsing`` () =

    let ifNoneMatchTyped =
        IfNoneMatch (IfNoneMatchChoice.EntityTags [ Strong "sometag"; Weak "othertag" ])

    let ifNoneMatchString =
        "\"sometag\",W/\"othertag\""

    roundTrip (IfNoneMatch.Format, IfNoneMatch.Parse) [
        ifNoneMatchTyped, ifNoneMatchString ]

[<Fact>]
let ``IfModifiedSince Formatting/Parsing`` () =

    let ifModifiedSinceTyped =
        IfModifiedSince (DateTime.Parse ("1994/10/29 19:43:31"))

    let ifModifiedSinceString =
        "Sat, 29 Oct 1994 19:43:31 GMT"

    roundTrip (IfModifiedSince.Format, IfModifiedSince.Parse) [
        ifModifiedSinceTyped, ifModifiedSinceString ]

[<Fact>]
let ``IfUnmodifiedSince Formatting/Parsing`` () =

    let ifUnmodifiedSinceTyped =
        IfUnmodifiedSince (DateTime.Parse ("1994/10/29 19:43:31"))

    let ifUnmodifiedSinceString =
        "Sat, 29 Oct 1994 19:43:31 GMT"

    roundTrip (IfUnmodifiedSince.Format, IfUnmodifiedSince.Parse) [
        ifUnmodifiedSinceTyped, ifUnmodifiedSinceString ]

(* RFC 7233 *)

[<Fact>]
let ``IfRange Formatting/Parsing`` () =

    let ifRangeTyped =
        IfRange (IfRangeChoice.Date (DateTime.Parse ("1994/10/29 19:43:31")))

    let ifRangeString =
        "Sat, 29 Oct 1994 19:43:31 GMT"

    roundTrip (IfRange.Format, IfRange.Parse) [
        ifRangeTyped, ifRangeString ]

(* RFC 7234 *)

[<Fact>]
let ``Age Formatting/Parsing`` () =

    let ageTyped =
        Age (TimeSpan.FromSeconds (float 1024))

    let ageString =
        "1024"

    roundTrip (Age.Format, Age.Parse) [
        ageTyped, ageString ]

[<Fact>]
let ``CacheControl Formatting/Parsing`` () =

    let cacheControlTyped =
        CacheControl [
            MaxAge (TimeSpan.FromSeconds (float 1024))
            NoCache
            Private ]

    let cacheControlString =
        "max-age=1024,no-cache,private"

    roundTrip (CacheControl.Format, CacheControl.Parse) [
        cacheControlTyped, cacheControlString ]

[<Fact>]
let ``Expires Formatting/Parsing`` () =

    let expiresTyped =
        Expires (DateTime.Parse ("1994/10/29 19:43:31"))

    let expiresString =
        "Sat, 29 Oct 1994 19:43:31 GMT"

    roundTrip (Expires.Format, Expires.Parse) [
        expiresTyped, expiresString ]