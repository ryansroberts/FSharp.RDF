#r "../../packages/dotNetRDF/lib/net40/dotNetRDF.dll"
#r "../../packages/VDS.Common/lib/net40-client/VDS.Common.dll"
#r "../../packages/FSharpx.Core/lib/40/FSharpx.Core.dll"
#r "../../packages/Unquote/lib/net40/Unquote.dll"
#load "Graph.fs"
#load "Store.fs"
#load "Assertion.fs"

open FSharp.RDF
open System.IO
open Swensen.Unquote

let functionalProperties = """
@prefix : <http://testing.stuff/ns#> .
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix xsd: <http://www.w3.org/2001/xmlschema#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@base <http://testing.stuff/ns> .

:item1 rdf:type :type1;
       :pr1 :item2 .

:item2 rdf:type :type2;
       :pr2 :item3 .

:item3 rdf:type :type3;
       :pr3 "avalue".

:item4 rdf:type :type4;
       :pr4 [
         rdf:type :type5;
         :pr3 "blankvalue";
        ] .
"""
let item1 = Uri.from "http://testing.stuff/ns#item1"
let type1 = Uri.from "http://testing.stuff/ns#type1"
let type2 = Uri.from "http://testing.stuff/ns#type2"
let item3 = Uri.from "http://testing.stuff/ns#item3"
let item4 = Uri.from "http://testing.stuff/ns#item4"
let pr1 = Uri.from "http://testing.stuff/ns#pr1"
let pr2 = Uri.from "http://testing.stuff/ns#pr2"
let pr3 = Uri.from "http://testing.stuff/ns#pr3"
let pr4 = Uri.from "http://testing.stuff/ns#pr4"

open Store
open resource

let g = Graph.from functionalProperties
let r1 = (fromSubject item1 g) |> List.head
let r3 = (fromSubject item3 g) |> List.head

let ``Pattern match id`` =
  test <@ true = match r1 with
                 | Is item1 _ -> true
                 | _ -> false @>

let ``Fail to pattern match id`` =
  test <@ match r1 with
          | Is item3 _ -> true
          | _ -> false @>

let ``Pattern match type`` =
  test <@ true = match r1 with
                 | HasType type1 _ -> true
                 | _ -> false @>

let ``Fail to pattern match type`` =
  test <@ false = match r1 with
                  | HasType type2 _ -> true
                  | _ -> false @>

let ``Map object`` =
  test <@ [ "avalue" ] = match r3 with
                         | DataProperty pr3 xsd.string values -> values
                         | _ -> [] @>

let ``Traverse an object property`` =
  test <@ [ true ] = [ for r in (fromSubject item1 g) do
                         match r with
                         | Property pr1 next ->
                           for r' in traverse next do
                             yield true ] @>

let ``Traverse a blank node`` =
  test <@ [ true ] = [ for r in (fromSubject item4 g) do
                         match r with
                         | Property pr4 next ->
                           for r' in traverse next do
                             yield true ] @>

open Assertion

let s = ""
let sb = new System.Text.StringBuilder(s)

let bob =
  resource !"base:id"
    [ a !"base:Type"
      objectProperty !"base:someObjectProperty" !"base:SomeOtherId"
      dataProperty !"base:someDataProperty" ("value" ^^^ xsd.string)

      blank !"base:someBlankProperty"
        [ a !"base:BankType"
          dataProperty !"base:someDataProperty" ("value2" ^^^ xsd.string) ]

      one !"base:someOtherObjectProperty" !"base:id2"
        [ a !"base:LinkedType"
          dataProperty !"base:someDataProperty" ("value3" ^^^ xsd.string) ] ]

[ bob ]
|> output.toGraph "http://sometest/ns#"
|> output.format output.ttl (output.toString sb)
string sb
