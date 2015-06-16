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