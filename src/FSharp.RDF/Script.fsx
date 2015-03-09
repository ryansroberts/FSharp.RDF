#r "../../packages/dotNetRDF/lib/net40/dotNetRDF.dll"
#r "../../packages/VDS.Common/lib/net40-client/VDS.Common.dll"
#r "../../packages/FSharpx.Core/lib/40/FSharpx.Core.dll"
#r "../../packages/Unquote/lib/net40/Unquote.dll"

#load "Uri.fs"
#load "ResultGraph.fs"
#load "Namespace.fs"
#load "Store.fs"

open Store
open Walk
open Swensen.Unquote

open Graph
let functionalProperties = """
@prefix : <http://testing.stuff/ns#> .
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@base <http://testing.stuff/ns> .

:item1 rdf:type :type1.
:item1 :pr1 :item2 .
:item1 :pr3 "1"^^xsd:string .

:item2 rdf:type :type2 .
:item2 :pr2 :item3 .
:item2 :pr3 "2"^^xsd:string .

:item3 rdf:type :type3.
:item3 :pr3 "3"^^xsd:string .
"""

let g = Graph.from functionalProperties
let item1 = Uri.from "http://testing.stuff/ns#item1"
let pr1 = Predicate.from "http://testing.stuff/ns#pr1"
let pr2 = Predicate.from "http://testing.stuff/ns#pr2"
let pr3 = Predicate.from "http://testing.stuff/ns#pr3"

open Combinators

let typeValue = walk {
  for s in (fromSubject item1 g) do
  wherePredicate pr1
  o Literal.mapString
  }


