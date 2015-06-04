# Arachne

Types for HTTP and related RFCs.

## Overview

Arachne is a library of types for working with HTTP and related standards. It contains data structures modelled on the RFCs for HTTP, URI, LanguageTag, plus other specs such as CORS, along with parsing and formatting code to reliably convert valid string representations to and from the Arachne types.

By having a fully defined data structure it becomes easy to pattern match over elements of complex types, or to use lenses to work with components of a complex type. It also gives you a level of confidence that your work with HTTP is reliable and safe - constructing an invalid instance of HTTP data using typed representations is a lot more difficult than working with raw strings!

Here's a couple of quick examples, showing the kinds of data structures contained and the results of parsing some simple values, in this case a URI and the value of an Accept header. Quite useful!

```fsharp

open Arachne.Http
open Arachne.Uri

Uri.Parse "http://www.example.com:80/path?query=str#fragment"

// =

Uri
  (Scheme "http",
   Authority
     (Authority (Name (RegName "www.example.com"), Some (Port 80), None),
      PathAbsoluteOrEmpty ["path"]), Some (Query "query=str"),
   Some (Fragment "fragment"))

// --

Accept.Parse "text/*, text/html;q=0.7, text/html;level=1, */*"

// =

Accept
  [ AcceptableMedia (Partial (Type "text", Parameters (map [])), None)
    AcceptableMedia (Closed (Type "text", SubType "html", Parameters (map [])), Some (AcceptParameters (Weight 0.7, Extensions (map []))))
    AcceptableMedia (Closed (Type "text", SubType "html", Parameters (map [ ("level", "1") ])), None)
    AcceptableMedia (Open (Parameters (map [])), None) ]

```

## Build status

| Platform |  BuildScript | Status of last build |
| :------ | :------: | :------: |
| **Mono** | [build.sh](https://github.com/freya-fs/arachne/blob/master/build.sh) | [![Travis build status](https://travis-ci.org/freya-fs/arachne.svg?branch=master)](https://travis-ci.org/freya-fs/arachne) |
| **Windows** | [build.cmd](https://github.com/freya-fs/arachne/blob/master/build.cmd) | [![AppVeyor build status](https://ci.appveyor.com/api/projects/status/9wk7ybt2q237h4l8/branch/master?svg=true)](https://ci.appveyor.com/project/freyafs/arachne/branch/master) |

[![Issue Stats][badge-issue-stats]][link-issue-stats] [![Pull Requests Stats][badge-pr-stats]][link-issue-stats]

## Packages

| Name | NuGet |
| :------ | :------: |
| Arachne (meta) | [![NuGet Status](http://img.shields.io/nuget/v/Arachne.svg?style=flat)](https://www.nuget.org/packages/Arachne/) |
| Arachne.Core | [![NuGet Status](http://img.shields.io/nuget/v/Arachne.Core.svg?style=flat)](https://www.nuget.org/packages/Arachne.Core/) |
| Arachne.Uri | [![NuGet Status](http://img.shields.io/nuget/v/Arachne.Uri.svg?style=flat)](https://www.nuget.org/packages/Arachne.Uri/) |
| Arachne.Uri.Template | [![NuGet Status](http://img.shields.io/nuget/v/Arachne.Uri.Template.svg?style=flat)](https://www.nuget.org/packages/Arachne.Uri.Template/) |
| Arachne.Language | [![NuGet Status](http://img.shields.io/nuget/v/Arachne.Language.svg?style=flat)](https://www.nuget.org/packages/Arachne.Language/) |
| Arachne.Http | [![NuGet Status](http://img.shields.io/nuget/v/Arachne.Http.svg?style=flat)](https://www.nuget.org/packages/Arachne.Http/) |
| Arachne.Http.Cors | [![NuGet Status](http://img.shields.io/nuget/v/Arachne.Http.Cors.svg?style=flat)](https://www.nuget.org/packages/Arachne.Http.Cors/) |

## Questions?

* Ask questions or submit issues on the [issue tracker](https://github.com/freya-fs/arachne/issues)
* [![Join the chat at https://gitter.im/freya-fs/arachne](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/freya-fs/arachne?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

## Maintainers

* [@kolektiv](https://github.com/kolektiv)
* [@panesofglass](https://github.com/panesofglass)

 [badge-pr-stats]: http://www.issuestats.com/github/freya-fs/arachne/badge/pr
 [badge-issue-stats]: http://www.issuestats.com/github/freya-fs/arachne/badge/issue
 [link-issue-stats]: http://www.issuestats.com/github/freya-fs/arachne
