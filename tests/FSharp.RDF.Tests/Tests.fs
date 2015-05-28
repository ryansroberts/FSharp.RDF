module FSharp.RDF.Tests

open FSharp.RDF
open Xunit
open System.IO
open Swensen.Unquote

let functionalProperties = """
@prefix : <http://testing.stuff/ns#> .
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix xsd: <http://www.w3.org/2001/xmlschema#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@base <http://testing.stuff/ns#> .

:item1 rdf:type :type1;
       :pr1 :item2 .

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

open resource

let g = Graph.loadTtl (fromString functionalProperties)
let r1 = (Resource.fromSubject item1 g) |> Seq.head
let r3 = (Resource.fromSubject item3 g) |> Seq.head

[<Fact>]
let ``Pattern match id``() =
  <@ true = match r1 with
            | Is item1 _ -> true
            | _ -> false @>

[<Fact>]
let ``Fail to pattern match id``() =
  <@ match r1 with
     | Is item3 _ -> true
     | _ -> false @>

[<Fact>]
let ``Pattern match type``() =
  test <@ true = match r1 with
                 | HasType type1 _ -> true
                 | _ -> false @>

[<Fact>]
let ``Fail to pattern match type``() =
  test <@ false = match r1 with
                  | HasType type2 _ -> true
                  | _ -> false @>

[<Fact>]
let ``Map object``() =
  test <@ [ "avalue" ] = match r3 with
                         | DataProperty pr3 xsd.string values -> values
                         | _ -> [] @>

[<Fact>]
let ``Traverse an object property``() =
  test <@ [ true ] = [ for r in (Resource.fromSubject item1 g) do
                         match r with
                         | Property pr1 next ->
                           for r' in traverse next do
                             yield true ] @>

[<Fact>]
let ``Traverse a blank node``() =
  test <@ [ true ] = [ for r in (Resource.fromSubject item4 g) do
                         match r with
                         | Property pr4 next ->
                           for r' in traverse next do
                             yield true ] @>

open Assertion

open rdf
[<Fact>]
let ``Assert a resource``() =
  let s = ""
  let sb = new System.Text.StringBuilder(s)

  let og = Graph.empty (!"http://sometest/ns#") [("base",!"http://sometest/ns#")]
  let r =
    resource !"base:id"
      [ a !"base:Type"
        objectProperty !"base:someObjectProperty" !"base:SomeOtherId"
        objectProperty !"base:someNonQname" !"http://google.com/stuff"
        dataProperty !"base:someDataProperty" ("value"^^xsd.string)

        blank !"base:someBlankProperty"
          [ a !"base:BankType"
            dataProperty !"base:someDataProperty" ("value2"^^xsd.string) ]

        one !"base:someOtherObjectProperty" !"base:id2"
          [ a !"base:LinkedType"
            dataProperty !"base:someDataProperty" ("value3"^^xsd.string) ]]
  [r]
  |> Assert.graph og
  |> Graph.writeTtl (toString sb)
  |> ignore

  let g = Graph.loadTtl (fromString (sb.ToString()))
  let g' = Graph.loadTtl (fromString """@base <http://sometest/ns#>.

@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>.
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#>.
@prefix xsd: <http://www.w3.org/2001/XMLSchema#>.
@prefix prov: <http://www.w3.org/ns/prov#>.
@prefix owl: <http://www.w3.org/2002/07/owl#>.
@prefix git2prov: <http://nice.org.uk/ns/prov/>.
@prefix base: <http://sometest/ns#>.
@prefix compilation: <http://nice.org.uk/ns/compilation#>.
@prefix cnt: <http://www.w3.org/2011/content#>.

base:id base:someBlankProperty [rdf:type base:BankType ;
                                    base:someDataProperty "value2"^^xsd:string];
          base:someDataProperty "value"^^xsd:string;
          base:someObjectProperty base:SomeOtherId;
          base:someOtherObjectProperty base:id2;
          base:someNonQname <http://google.com/stuff>;
          rdf:type base:Type.
base:id2 base:someDataProperty "value3"^^xsd:string;
           rdf:type base:LinkedType.
""")

  (Graph.diff g g').AreEqual =? true


[<Fact>]
let ``Streaming resources`` () =
  let s = ""
  let sb = new System.Text.StringBuilder(s)

  let g = Graph.empty (!"http://sometest/ns#") [("base",!"http://sometest/ns#")]
  let r =
    resource !"http://an.id" [
      a !"base:type"
      blank !"base:someBlankProperty"
          [ a !"base:BankType"
            dataProperty !"base:someDataProperty" ("value1"^^xsd.string) ]

      blank !"base:someBlankProperty"
          [ a !"base:BankType"
            dataProperty !"base:someDataProperty" ("value2"^^xsd.string) ]
      ]
  [r]
  |> Assert.triples g
  |> Graph.streamTtl g (toString sb)
  |> Seq.iter (fun _ -> ())

  let g' = Graph.loadTtl (fromString (sb.ToString()))
  (Graph.diff g g').AreEqual =? true
