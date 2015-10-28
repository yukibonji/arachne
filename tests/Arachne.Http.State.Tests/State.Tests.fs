module Arachne.Http.State.Tests

open System
open NUnit.Framework
open Arachne.Core.Tests
open Arachne.Http.State
open Arachne.Uri

[<Test>]
let ``Cookie Formatting/Parsing`` () =
    let cookieTyped =
        Cookie [
            CookiePair (
                CookieName "test",
                CookieValue "value")]

    let cookieString =
        "test=value"

    roundTrip (Cookie.Format, Cookie.Parse) [
        cookieTyped, cookieString ]

[<Test>]
let ``Multiple Cookie Formatting/Parsing`` () =
    let cookieTyped =
        Cookie [
            CookiePair (
                CookieName "cookie1",
                CookieValue "foo")
            CookiePair (
                CookieName "cookie2",
                CookieValue "bar")]

    let cookieString =
        "cookie1=foo; cookie2=bar"

    roundTrip (Cookie.Format, Cookie.Parse) [
        cookieTyped, cookieString ]

[<Test>]
let ``Set-Cookie Formatting/Parsing`` () =
    let setCookieTyped =
        SetCookie (
            CookiePair (
                CookieName "test",
                CookieValue "value"),
            CookieAttributes [
                Expires (DateTime.Parse "1994/10/29 19:43:31")
                MaxAge (TimeSpan.FromSeconds 42.)
                Domain (SubDomain "www.example.com")
                CookieAttribute.Path (Path "/hello")
                Secure
                HttpOnly ])

    let setCookieString =
        "test=value; Expires=Sat, 29 Oct 1994 19:43:31 GMT; Max-Age=42; Domain=www.example.com; Path=/hello; Secure; HttpOnly"

    roundTrip (SetCookie.Format, SetCookie.Parse) [
        setCookieTyped, setCookieString ]