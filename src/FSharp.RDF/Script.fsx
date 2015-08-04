#r "../../packages/dotNetRDF/lib/net40/dotNetRDF.dll"
#r "../../packages/VDS.Common/lib/net40-client/VDS.Common.dll"
#r "../../packages/FSharpx.Core/lib/40/FSharpx.Core.dll"
#r "../../packages/Unquote/lib/net40/Unquote.dll"
#r "../../packages/json-ld.net/lib/net40-Client/JsonLD.dll"
#I "../../packages/Newtonsoft.Json/lib/net40/"
#r "../../packages/HtmlAgilityPack/lib/Net40/HtmlAgilityPack.dll"
#r "../../packages/Newtonsoft.Json/lib/net40/Newtonsoft.Json.dll"

#load "Graph.fs"
#load "JsonLd.fs"
open FSharp.RDF
#load "ResultSet.fs"

open FSharp.RDF
open System.IO
open Swensen.Unquote
open Store

let functionalProperties = """
@prefix : <http://testing.stuff/ns#> .
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix xsd: <http://www.w3.org/2001/xmlschema#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@base <http://testing.stuff/ns#> .
@prefix testing: <http://testing.stuff/ns#> .

:item1 rdf:type :type1;
       testing:pr1 :item2 .

:item2 rdf:type :type2;
       :pr2 :item3 .

:item3 rdf:type :type3;
       :pr3 "avalue"^^xsd:string .

:item4 rdf:type :type4;
       :pr4 [
         rdf:type :type5;
         :pr3 "blankvalue"^^xsd:string ;
        ] .
"""
let item1 = Uri.from "http://testing.stuff/ns#item1"
let type1 = Uri.from "http://testing.stuff/ns#type1"
let type2 = Uri.from "base:type2"
let item3 = Uri.from "http://testing.stuff/ns#item3"
let item4 = Uri.from "http://testing.stuff/ns#item4"
let pr1 = Uri.from "http://testing.stuff/ns#pr1"
let pr2 = Uri.from "http://testing.stuff/ns#pr2"
let pr3 = Uri.from "http://testing.stuff/ns#pr3"
let pr4 = Uri.from "http://testing.stuff/ns#pr4"

let qn1 = Uri.from "testing:pr1"

open resource

let g = Graph.loadTtl (fromString functionalProperties)
        |> Graph.addPrefixes (Uri.from "http://base/") [("testing",Uri.from "http://testing.stuff/ns#")]

let r1 = (Resource.fromSubject item1 g) |> Seq.head
let r3 = (Resource.fromSubject item3 g) |> Seq.head

