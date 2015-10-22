module Arachne.Http.State.Tests

open System
open NUnit.Framework
open Arachne.Core.Tests
open Arachne.Http.State

[<Test>]
let ``Cookie Formatting/Parsing`` () =
    let cookieTyped =
        Cookie (
            CookiePair (
                CookieName "test",
                CookieValue "value"))

    let cookieString =
        "test=value"

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
                Secure
                HttpOnly ])

    let setCookieString =
        "test=value; Expires=Sat, 29 Oct 1994 19:43:31 GMT; Secure; HttpOnly"

    roundTrip (SetCookie.Format, SetCookie.Parse) [
        setCookieTyped, setCookieString ]