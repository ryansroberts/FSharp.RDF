module FSharp.RDF.Tests

open FSharp.RDF
open NUnit.Framework
open Uri
open Traversal
open System.IO

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
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@base <http://testing.stuff/ns> .

:item1 rdf:type :type1;
       :pr1 :item2 .

:item2 rdf:type :type2;
       :pr2 :item3 .

:item3 rdf:type :type3;
       :pr3 "avalue"^^xsd:string
"""

[<Test>]
let ``Traverse functional properties`` () =
  let g = Graph.from functionalProperties
  ()
