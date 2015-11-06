open Arachne.Uri.Template

[<EntryPoint>]
let main _ =

    let uri = UriTemplate.Parse "/hello{?keys}"

    0 // return an integer exit code
