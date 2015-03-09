module FSharp.RDF.Tests

open NUnit.Framework
open Graph
open System.IO
open Swensen.Unquote

(*Fucking about*)

(* open System
type SomeRec =
  {
    SomeOptionalProp : string
    SomeProp : string
    SomeMutipleProp : string list
    }

let sx = (Subject.from (System.Uri "http://ns/res1"),[
(Predicate.from (System.Uri "http://ns/pr1"),Object.from (System.Uri "http://ns/bob"))
(Predicate.from (System.Uri "http://ns/pr2"),Object.from (System.Uri "http://ns/jim"))
(Predicate.from (System.Uri "http://ns/pr2"),Object.from (System.Uri "http://ns/john"))
])


let tx = (Subject.from (System.Uri "http://subject"),Predicate.from (System.Uri "http://predicate"),Object.from (System.Uri "http://object"))

let objectUri = [tx] <--*> string

let pr1 = Predicate.from (System.Uri "http://ns/pr1" )
let pr2 = Predicate.from (System.Uri "http://ns/pr1" )
let pr3 = Predicate.from (System.Uri "http://ns/pr3" )
let tMany = [sx] ==> pr1

let tFunctional = [sx] ==> pr1 ==> pr2

let mPr1 = [sx] ==> pr1
                ==> pr2
                .> pr3
                <--*> string

                *)

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

[<Test>]
let ``Extract from data property of resource``() =
  let g = Graph.from functionalProperties
  let root = fromSubject item3 g
  let item1P = walk {
    for s in root do
    o pr3 Literal.mapString
    }
  let rx = parsers.run item1P []

  test <@ Some [Some "avalue"] = rx @>

[<Test>]
let ``Traverse object property that is not asserted``() =
  let g = Graph.from functionalProperties
  let root = fromSubject item3 g
  let item1P = walk {
    for s in root do
    traverse pr1
    }
  let rx = parsers.run item1P []

  test <@ [] = rx @>


