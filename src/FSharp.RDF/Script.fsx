#r "../../packages/dotNetRDF/lib/net40/dotNetRDF.dll"
#r "../../packages/VDS.Common/lib/net40-client/VDS.Common.dll"
#r "../../packages/FSharpx.Core/lib/40/FSharpx.Core.dll"
#r "../../packages/Unquote/lib/net40/Unquote.dll"
#load "Uri.fs"
#load "ResultGraph.fs"
#load "Namespace.fs"
#load "Store.fs"

open Graph
open Store
open Traversal
open Swensen.Unquote

let functionalProperties = """
@prefix : <http://testing.stuff/ns#> .
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@base <http://testing.stuff/ns> .

:item1 rdf:type :type1.
:item1 :pr1 :item2 .

:item2 rdf:type :type2 .
:item2 :pr2 :item3 .

:item3 rdf:type :type3.
:item3 :pr3 "avalue"^^xsd:string .
"""
let g = Graph.from functionalProperties
let s = Uri.from "http://testing.stuff/ns#item1"
let pr1 = Predicate.from "http://testing.stuff/ns#pr1"
let pr2 = Predicate.from "http://testing.stuff/ns#pr2"
let pr3 = Predicate.from "http://testing.stuff/ns#pr3"
let sx = fromSubject s g
let avalue = [ sx ] ==> pr1 ==> pr2 .> pr3 <--*> Literal.mapString
