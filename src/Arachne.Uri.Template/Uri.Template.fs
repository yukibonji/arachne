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

module Arachne.Uri.Template

open System.Text
open Arachne.Core
open Arachne.Uri
open FParsec

(* RFC 6570

   Types, parsers and formatters implemented to mirror the specification of 
   URI Template semantics as defined in RFC 6570.

   Taken from [http://tools.ietf.org/html/rfc6570] *)

(* Grammar

   NOTE: We do not currently support IRIs - this may
   be supported in future. *)

[<RequireQualifiedAccess>]
module internal Grammar =

    let isLiteral i =
            i = 0x21
         || i >= 0x23 && i <= 0x24
         || i = 0x26
         || i >= 0x28 && i <= 0x3b
         || i = 0x3d
         || i >= 0x3f && i <= 0x5b
         || i = 0x5d
         || i = 0x5f
         || i >= 0x61 && i <= 0x7a
         || i = 0x7e

    let isVarchar i =
            Grammar.isAlpha i
         || Grammar.isDigit i
         || i = 0x5f // _

(* Aliases *)

module F = Formatting
module G = Grammar
module M = Mapping

(* Data

   Types representing data which may be rendered or extracted
   using UriTemplates. *)

type UriTemplateData =
    | UriTemplateData of Map<UriTemplateKey, UriTemplateValue>

    static member uriTemplateData_ =
        (fun (UriTemplateData x) -> x), (UriTemplateData)

    static member (+) (UriTemplateData a, UriTemplateData b) =
        UriTemplateData (Map.ofList (Map.toList a @ Map.toList b))

 and UriTemplateKey =
    | Key of string

 and UriTemplateValue =
    | Atom of string
    | List of string list
    | Keys of (string * string) list

    static member atom_ =
        (function | Atom x -> Some x | _ -> None), (Atom)

    static member list_ =
        (function | List x -> Some x | _ -> None), (List)

    static member keys_ =
        (function | Keys x -> Some x | _ -> None), (Keys)

(* Matching *)

type Matching<'a,'b> =
    { Match: Match<'a,'b> }

and Match<'a,'b> =
    'a -> Parser<'b, unit>

(* Rendering

   Types and functions to support a general concept of a type rendering
   itself given some state data d', producing a rendering concept much
   like the Format concept, but with readable state. *)

type Rendering<'a> =
    { Render: Render<'a> }

and Render<'a> =
    UriTemplateData -> 'a -> StringBuilder -> StringBuilder

[<AutoOpen>]
module private Functions =

    let match' (m: Match<'a,'b>) s a =
        match run (m a) s with
        | Success (x, _, _) -> x
        | Failure (e, _, _) -> failwith e

    let render (render: Render<'a>) =
        fun d a -> string (render d a (StringBuilder ()))

(* Parsers

   Some extra functions for parsing, in particular for dynamically
   parsing using a list of dynamically constructed parsers which should
   succeed or fail as a single parser. *)

[<AutoOpen>]
module private Parsers =

    let multi parsers =
        fun stream ->
            let rec eval state =
                match state with
                | vs, [] ->
                    Reply (vs)
                | vs, p :: ps ->
                    match p stream with
                    | (x: Reply<'a>) when x.Status = Ok -> eval (x.Result :: vs, ps)
                    | (x) -> Reply<'a list> (Status = x.Status, Error = x.Error)

            eval ([], parsers)

    let multiSepBy parsers sep =
        fun stream ->
            let rec eval state =
                match state with
                | _, vs, [] ->
                    Reply (vs)
                | true, vs, ps ->
                    match sep stream with
                    | (x: Reply<unit>) when x.Status = Ok -> eval (false, vs, ps)
                    | (x) -> Reply<'a list> (Status = x.Status, Error = x.Error)
                | false, vs, p :: ps ->
                    match p stream with
                    | (x: Reply<'a>) when x.Status = Ok -> eval (true, x.Result :: vs, ps)
                    | (x) -> Reply<'a list> (Status = x.Status, Error = x.Error)

            eval (false, [], parsers)

(* Template

   Taken from RFC 6570, Section 2 Syntax
   See [http://tools.ietf.org/html/rfc6570#section-2] *)

type UriTemplate =
    | UriTemplate of UriTemplatePart list

    static member Mapping =

        let uriTemplateP =
            many1 UriTemplatePart.Mapping.Parse |>> UriTemplate

        let uriTemplateF =
            function | UriTemplate u -> F.join UriTemplatePart.Mapping.Format id u

        { Parse = uriTemplateP
          Format = uriTemplateF }

    static member Matching =

        let uriTemplateM =
            function | UriTemplate parts ->
                        multi (List.map UriTemplatePart.Matching.Match parts)
                        |>> List.fold (+) (UriTemplateData Map.empty)

        { Match = uriTemplateM }

    static member Rendering =

        let uriTemplateR (data: UriTemplateData) =
            function | UriTemplate p -> F.join (UriTemplatePart.Rendering.Render data) id p

        { Render = uriTemplateR }

    static member format =
        M.format UriTemplate.Mapping

    static member parse =
        M.parse UriTemplate.Mapping

    static member tryParse =
        M.tryParse UriTemplate.Mapping

    static member (+) (UriTemplate x, UriTemplate y) =
        match List.rev x, y with
        | (UriTemplatePart.Literal (Literal x) :: xs),
          (UriTemplatePart.Literal (Literal y) :: ys) ->
            UriTemplate (List.rev xs @ [ UriTemplatePart.Literal (Literal (x + y)) ] @ ys)
        | _ ->
            UriTemplate (x @ y)

    override x.ToString () =
        UriTemplate.format x

    member x.Match uri =
        match' UriTemplate.Matching.Match uri x

    member x.Render data =
        render UriTemplate.Rendering.Render data x

 and UriTemplatePart =
    | Literal of Literal
    | Expression of Expression

    static member Mapping =

        let uriTemplatePartP =
            (Expression.Mapping.Parse |>> Expression) <|> (Literal.Mapping.Parse |>> Literal)

        let uriTemplatePartF =
            function | Literal l -> Literal.Mapping.Format l
                     | Expression e -> Expression.Mapping.Format e

        { Parse = uriTemplatePartP
          Format = uriTemplatePartF }

    static member Matching =

        let uriTemplatePartM =
            function | Literal l -> Literal.Matching.Match l
                     | Expression e -> Expression.Matching.Match e

        { Match = uriTemplatePartM }

    static member Rendering =

        let uriTemplatePartR data =
            function | Literal l -> Literal.Rendering.Render data l
                     | Expression e-> Expression.Rendering.Render data e

        { Render = uriTemplatePartR }

    static member format =
        M.format UriTemplatePart.Mapping

    override x.ToString () =
        UriTemplatePart.format x

    member x.Match part =
        match' UriTemplatePart.Matching.Match part x

 and Literal =
    | Literal of string

    static member Mapping =

        // TODO: Consider where isomorphisms are now required...

        let parser =
            PercentEncoding.makeParser G.isLiteral

        let formatter =
            PercentEncoding.makeFormatter ()

        let literalP =
            notEmpty parser |>> Literal.Literal

        let literalF =
            function | Literal l -> formatter l

        { Parse = literalP
          Format = literalF }

    static member Matching =

        let literalM =
            function | Literal l -> pstring l >>% UriTemplateData Map.empty

        { Match = literalM }

    static member Rendering =

        let literalR _ =
            function | Literal l -> F.append l

        { Render = literalR }

 and Expression =
    | Expression of Operator option * VariableList

    static member Mapping =

        let expressionP =
            between 
                (skipChar '{') (skipChar '}') 
                (opt Operator.Mapping.Parse .>>. VariableList.Mapping.Parse)
                |>> Expression

        let expressionF =
            function | Expression (Some o, v) ->
                           F.append "{"
                        >> Operator.Mapping.Format o
                        >> VariableList.Mapping.Format v
                        >> F.append "}"
                     | Expression (_, v) ->
                           F.append "{"
                        >> VariableList.Mapping.Format v
                        >> F.append "}"

        { Parse = expressionP
          Format = expressionF }

    static member Matching =

        (* Primitives *)

        let idP =
            preturn ()

        let simpleP =
            let parser = PercentEncoding.makeParser G.isUnreserved
            let decoder = PercentEncoding.makeDecoder ()

            parser |>> decoder

        let isReserved i =
                G.isReserved i
             || G.isUnreserved i

        let reservedP =
            let parser = PercentEncoding.makeParser isReserved
            let decoder = PercentEncoding.makeDecoder ()

            parser |>> decoder

        (* Characters *)

        let commaP =
            skipChar ','

        let dotP =
            skipChar '.'

        let hashP =
            skipChar '#'

        let semicolonP =
            skipChar ';'

        let slashP =
            skipChar '/'

        let questionmarkP =
            skipChar '?'

        let ampersandP =
            skipChar '&'

        let equalsP =
            skipChar '='

        (* Values *)

        let atomP p key =
            p |>> fun s -> key, Atom s

        let listP p sep =
            sepBy1 p sep |>> List

        let keysP p sep =
            sepBy1 (p .>> equalsP .>>. p) sep |>> Keys

        let listOrKeysP p sep key =
            attempt (keysP p sep) <|> listP p sep |>> fun v -> key, v

        let namedListP p (Key name) =
            skipString name >>. opt (equalsP >>. (sepBy1 p commaP))

        let emptyList =
            function | None   -> [""]
                     | Some v -> v

        let atomOrList =
            function | [s] -> Atom s
                     | v   -> List v

        let namedP p key =
            namedListP p key |>> emptyList |>> atomOrList |>> fun v -> key, v

        let namedExplodedP p sep key =
            sepBy1 (namedListP p key) sep |>> List.collect emptyList |>> List

        let namedExplodedListOrKeysP p sep key =
            attempt (namedExplodedP p sep key) <|> keysP p sep |>> fun v -> key, v

        (* Mapping *)

        let mapVar key separator =
            function | Some (Level3 Query), Some (Level4 Explode)
                     | Some (Level3 QueryContinuation), Some (Level4 Explode) -> namedExplodedListOrKeysP simpleP separator key
                     | Some (Level3 Query), _
                     | Some (Level3 QueryContinuation), _ -> namedP simpleP key
                     | Some (Level2 _), Some (Level4 Explode) -> listOrKeysP reservedP separator key
                     | _, Some (Level4 Explode) -> listOrKeysP simpleP separator key
                     | Some (Level2 _), _ -> atomP reservedP key
                     | _ -> atomP simpleP key

        let mapVars operator separator (VariableList vars) =
            multiSepBy (List.map (fun (VariableSpec (VariableName name, modifier)) ->
                mapVar (Key name) separator (operator, modifier)) vars) separator

        let optMapVars operator separator vars =
            opt (mapVars operator separator vars) |>> function | Some x -> x
                                                               | None   -> []

        let mapExpression =
                function | Expression (None, vars) -> idP, mapVars None commaP vars
                         | Expression (Some (Level2 Reserved), vars) -> idP, mapVars (Some (Level2 Reserved)) commaP vars
                         | Expression (Some (Level2 Fragment), vars) -> hashP, mapVars (Some (Level2 Fragment)) commaP vars
                         | Expression (Some (Level3 Label), vars) -> dotP, mapVars (Some (Level3 Label)) dotP vars
                         | Expression (Some (Level3 Segment), vars) -> slashP, mapVars (Some (Level3 Segment)) slashP vars
                         | Expression (Some (Level3 Parameter), vars) -> semicolonP, mapVars (Some (Level3 Parameter)) semicolonP vars
                         | Expression (Some (Level3 Query), vars) -> questionmarkP, optMapVars (Some (Level3 Query)) ampersandP vars
                         | Expression (Some (Level3 QueryContinuation), vars) -> ampersandP, optMapVars (Some (Level3 QueryContinuation)) ampersandP vars
                         | _ -> failwith ""
             >> fun (prefixP, valuesP) -> opt (prefixP >>. valuesP)

        let expressionM e =
            mapExpression e |>> function | Some vars -> UriTemplateData (Map.ofList vars)
                                         | _ -> UriTemplateData (Map.empty)
        { Match = expressionM }

    static member Rendering =

        (* Expansion *)

        let crop (s: string) length =
            s.Substring (0, min length s.Length)

        let expandUnary f s =
            function | (_, Atom "", _)
                     | (_, List [], _)
                     | (_, Keys [], _) -> id
                     | (_, Atom a, Some (Level4 (Prefix i))) -> f (crop a i)
                     | (_, Atom a, _) -> f a
                     | (_, List l, Some (Level4 Explode)) -> F.join f s l
                     | (_, List l, _) -> F.join f (F.append ",") l
                     | (_, Keys k, Some (Level4 Explode)) -> F.join (fun (k, v) -> f k >> F.append "=" >> f v) s k
                     | (_, Keys k, _) -> F.join (fun (k, v) -> f k >> F.append "," >> f v) (F.append ",") k

        let expandBinary f s omit =
            function | (n, Atom x, _) when omit x -> f n
                     | (n, List [], _)
                     | (n, Keys [], _) -> f n
                     | (n, Atom a, Some (Level4 (Prefix i))) -> f n >> F.append "=" >> f (crop a i)
                     | (n, Atom a, _) -> f n >> F.append "=" >> f a
                     | (n, List l, Some (Level4 Explode)) -> F.join (fun v -> f n >> F.append "=" >> f v) s l
                     | (n, List l, _) -> f n >> F.append "=" >> F.join f (F.append ",") l
                     | (_, Keys k, Some (Level4 Explode)) -> F.join (fun (k, v) -> f k >> F.append "=" >> f v) s k
                     | (n, Keys k, _) -> f n >> F.append "=" >> F.join (fun (k, v) -> f k >> F.append "," >> f v) (F.append ",") k

        (* Filtering *)

        let choose (VariableList variableList) (UriTemplateData data) =
            variableList
            |> List.map (fun (VariableSpec (VariableName n, m)) ->
                match Map.tryFind (Key n) data with
                | None
                | Some (List [])
                | Some (Keys []) -> None
                | Some v -> Some (n, v, m))
            |> List.choose id

        (* Rendering *)

        let render f variableList data =
            match choose variableList data with
            | [] -> id
            | data -> f data

        let renderUnary prefix item sep =
            render (fun x -> prefix >> F.join (expandUnary item sep) sep x)

        let renderBinary prefix item sep omit =
            render (fun x -> prefix >> F.join (expandBinary item sep omit) sep x)

        (* Simple Expansion *)

        let simpleF =
            let encoder = PercentEncoding.makeEncoder G.isUnreserved
            let formatter = PercentEncoding.makeFormatter ()

            encoder >> formatter

        let simpleExpansion =
            renderUnary id simpleF (F.append ",")

        (* Reserved Expansion *)

        let isReserved i =
                G.isReserved i
             || G.isUnreserved i

        let reservedF =
            let encoder = PercentEncoding.makeEncoder isReserved
            let formatter = PercentEncoding.makeFormatter ()

            encoder >> formatter

        let reservedExpansion =
            renderUnary id reservedF (F.append ",")

        (* Fragment Expansion *)

        let fragmentExpansion =
            renderUnary (F.append "#") reservedF (F.append ",")

        (* Label Expansion with Label-Prefix *)

        let labelExpansion =
            renderUnary (F.append ".") simpleF (F.append ".")

        (* Path Segment Expansion *)

        let segmentExpansion =
            renderUnary (F.append "/") simpleF (F.append "/")

        (* Parameter Expansion *)

        let parameterExpansion =
            renderBinary (F.append ";") simpleF (F.append ";") ((=) "")

        (* Query Expansion *)

        let queryExpansion =
            renderBinary (F.append "?") simpleF (F.append "&") (fun _ -> false)

        (* Query Continuation Expansion *)

        let queryContinuationExpansion =
            renderBinary (F.append "&") simpleF (F.append "&") (fun _ -> false)

        (* Expression *)

        let expressionR data =
            function | Expression (None, v) -> simpleExpansion v data
                     | Expression (Some (Level2 Reserved), v) -> reservedExpansion v data
                     | Expression (Some (Level2 Fragment), v) -> fragmentExpansion v data
                     | Expression (Some (Level3 Label), v) -> labelExpansion v data
                     | Expression (Some (Level3 Segment), v) -> segmentExpansion v data
                     | Expression (Some (Level3 Parameter), v) -> parameterExpansion v data
                     | Expression (Some (Level3 Query), v) -> queryExpansion v data
                     | Expression (Some (Level3 QueryContinuation), v) -> queryContinuationExpansion v data
                     | _ -> id

        { Render = expressionR }

(* Operators

   Taken from RFC 6570, Section 2.2 Expressions
   See [http://tools.ietf.org/html/rfc6570#section-2.2] *)

 and Operator =
    | Level2 of OperatorLevel2
    | Level3 of OperatorLevel3
    | Reserved of OperatorReserved

    static member Mapping =

        let operatorP =
            choice [
                OperatorLevel2.Mapping.Parse |>> Level2
                OperatorLevel3.Mapping.Parse |>> Level3
                OperatorReserved.Mapping.Parse |>> Reserved ]

        let operatorF =
            function | Level2 o -> OperatorLevel2.Mapping.Format o
                     | Level3 o -> OperatorLevel3.Mapping.Format o
                     | Reserved o -> OperatorReserved.Mapping.Format o

        { Parse = operatorP
          Format = operatorF }

 and OperatorLevel2 =
    | Reserved
    | Fragment

    static member Mapping =

        let operatorLevel2P =
            choice [
                skipChar '+' >>% Reserved
                skipChar '#' >>% Fragment ]

        let operatorLevel2F =
            function | Reserved -> F.append "+"
                     | Fragment -> F.append "#"

        { Parse = operatorLevel2P
          Format = operatorLevel2F }

 and OperatorLevel3 =
    | Label
    | Segment
    | Parameter
    | Query
    | QueryContinuation

    static member Mapping =

        let operatorLevel3P =
            choice [
                skipChar '.' >>% Label
                skipChar '/' >>% Segment
                skipChar ';' >>% Parameter
                skipChar '?' >>% Query
                skipChar '&' >>% QueryContinuation ]

        let operatorLevel3F =
            function | Label -> F.append "."
                     | Segment -> F.append "/"
                     | Parameter -> F.append ";"
                     | Query -> F.append "?"
                     | QueryContinuation -> F.append "&"

        { Parse = operatorLevel3P
          Format = operatorLevel3F }

 and OperatorReserved =
    | Equals
    | Comma
    | Exclamation
    | At
    | Pipe

    static member Mapping =

        let operatorReservedP =
            choice [
                skipChar '=' >>% Equals
                skipChar ',' >>% Comma
                skipChar '!' >>% Exclamation
                skipChar '@' >>% At
                skipChar '|' >>% Pipe ]

        let operatorReservedF =
            function | Equals -> F.append "="
                     | Comma -> F.append ","
                     | Exclamation -> F.append "!"
                     | At -> F.append "@"
                     | Pipe -> F.append "!"

        { Parse = operatorReservedP
          Format = operatorReservedF }

(* Variables

   Taken from RFC 6570, Section 2.3 Variables
   See [http://tools.ietf.org/html/rfc6570#section-2.3] *)

 and VariableList =
    | VariableList of VariableSpec list

    static member Mapping =

        let variableListP =
            sepBy1 VariableSpec.Mapping.Parse (skipChar ',')
            |>> VariableList

        let variableListF =
            function | VariableList v -> F.join VariableSpec.Mapping.Format (F.append ",") v

        { Parse = variableListP
          Format = variableListF }

 and VariableSpec =
    | VariableSpec of VariableName * Modifier option

    static member Mapping =

        let variableSpecP =
            VariableName.Mapping.Parse .>>. opt Modifier.Mapping.Parse
            |>> VariableSpec

        let variableSpecF =
            function | VariableSpec (name, Some m) ->
                           VariableName.Mapping.Format name
                        >> Modifier.Mapping.Format m
                     | VariableSpec (name, _) ->
                        VariableName.Mapping.Format name

        { Parse = variableSpecP
          Format = variableSpecF }

 and VariableName =
    | VariableName of string

    static member Mapping =

        // TODO: Assess the potential non-compliance
        // with percent encoding in variable names, especially
        // in cases which could involve percent encoded "." characters,
        // which would not play well with our over-naive formatting here
        // (which should potentially be reworked, although we are trying
        // to avoid keys having list values...)

        let parser =
            PercentEncoding.makeParser G.isVarchar

        let formatter =
            PercentEncoding.makeFormatter ()

        let variableNameP =
            sepBy1 (notEmpty parser) (skipChar '.')
            |>> ((String.concat ".") >> VariableName)

        let variableNameF =
            function | VariableName n ->
                        F.join formatter (F.append ".") (List.ofArray (n.Split ([| '.' |])))

        { Parse = variableNameP
          Format = variableNameF }

(* Modifiers

   Taken from RFC 6570, Section 2.4 Value Modifiers
   See [http://tools.ietf.org/html/rfc6570#section-2.4] *)

 and Modifier =
    | Level4 of ModifierLevel4

    static member Mapping =

        let modifierP =
            ModifierLevel4.Mapping.Parse |>> Level4

        let modifierF =
            function | Level4 m -> ModifierLevel4.Mapping.Format m

        { Parse = modifierP
          Format = modifierF }

 and ModifierLevel4 =
    | Prefix of int
    | Explode

    static member Mapping =

        let modifierLevel4P =
            choice [
                skipChar ':' >>. pint32 |>> Prefix
                skipChar '*' >>% Explode ]

        let modifierLevel4F =
            function | Prefix i -> F.appendf1 ":{0}" i
                     | Explode -> F.append "*"

        { Parse = modifierLevel4P
          Format = modifierLevel4F }