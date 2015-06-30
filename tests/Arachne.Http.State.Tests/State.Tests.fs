module Arachne.Http.State.Tests

open NUnit.Framework
open Arachne.Core.Tests
open Arachne.Http.State

[<Test>]
let ``Cookie Formatting/Parsing`` () =
    let cookieTyped =
        Cookie (CookieName "test", CookieValue "value")

    let cookieString =
        "test=value"

    roundTrip (Cookie.Format, Cookie.Parse) [
        cookieTyped, cookieString ]

[<Test>]
let ``Set-Cookie Formatting/Parsing`` () =
    let setCookieTyped =
        SetCookie (CookieName "test", CookieValue "value", CookieAttributes [ Secure ])

    let setCookieString =
        "test=value; Secure"

    roundTrip (SetCookie.Format, SetCookie.Parse) [
        setCookieTyped, setCookieString ]