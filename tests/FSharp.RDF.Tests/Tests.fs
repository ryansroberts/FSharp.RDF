module FSharp.RDF.Tests

open Xunit
open Graph
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
       :pr3 "avalue"^^xsd:string .
"""

let someOfProperties = """
@prefix : <http://testing.stuff/ns#> .
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@base <http://testing.stuff/ns> .

:item1 rdf:type :type1;
       :pr1 :item2 .

:item2 rdf:type :type2;
       :pr2 :item3 ;
       :pr2 :item4 ;
       :pr2 :item5 .

:item3 rdf:type :type3;
       :pr3 "1"^^xsd:string .
:item4 rdf:type :type3;
       :pr3 "2"^^xsd:string .
:item5 rdf:type :type3;
       :pr3 "3"^^xsd:string .
"""
let item1 = Uri.from "http://testing.stuff/ns#item1"
let item3 = Uri.from "http://testing.stuff/ns#item3"
let pr1 = Predicate.from "http://testing.stuff/ns#pr1"
let pr2 = Predicate.from "http://testing.stuff/ns#pr2"
let pr3 = Predicate.from "http://testing.stuff/ns#pr3"

open Store
open Walk

[<Fact>]
let ``Extract from data property of resource``() =
  let g = Graph.from functionalProperties
  let root = fromSubject item3 g
  let item1P = walk {
    mapO pr3 xsd.string
    }
  let rx = walker.run item1P root

  test <@ rx = Seq.empty @>

[<Fact>]
let ``Traverse object property that is not asserted``() =
  let g = Graph.from functionalProperties
  let root = fromSubject item3 g
  let item1P = walk {
    traverse pr1
    mapO pr3 xsd.string
    }
  let rx = walker.run item1P root

  test <@ rx = Seq.empty @>

[<Fact>]
let ``One or more combinator succeeds on match`` () =
  let g = Graph.from functionalProperties
  let root = fromSubject item3 g
  let item1P = walk {
    traverse pr1
    mapO pr3 xsd.string
    }

  let oneOrMoreItem1P = Combinators.oneOrMore item1P
  let rx = walker.run oneOrMoreItem1P root

  test <@ rx = Seq.empty @>
