#r "../../packages/FSharpx.Core/lib/40/FSharpx.Core.dll"
#r "../../packages/Unquote/lib/net40/Unquote.dll"
#r "../../packages/json-ld.net/lib/net40-Client/JsonLD.dll"
#I "../../packages/Newtonsoft.Json/lib/net40/"
#r "../../packages/HtmlAgilityPack/lib/Net40/HtmlAgilityPack.dll"
#r "../../packages/Newtonsoft.Json/lib/net40/Newtonsoft.Json.dll"
#I "../../lib/owlapi.net_release_1_0_0/"
#r "../../lib/owlapi.net_release_1_0_0/IKVM.OpenJDK.Core.dll"
#r "../../lib/owlapi.net_release_1_0_0/owlapi.dll"
#r "../../lib/owlapi.net_release_1_0_0/Cognitum.OwlApi.Net.ReasonerInterface.dll"
#r "../../lib/owlapi.net_release_1_0_0/Reasoners/Pellet/Cognitum.OwlApi.Net.Pellet.dll"
#I "../../lib/owlapi.net_release_1_0_0/Reasoners/Pellet"
#r "../../lib/owlapi.net_release_1_0_0/Reasoners/Pellet/pellet.dll"


#I "../../bin"
#r "../../packages/dotNetRDF/lib/net40/dotNetRDF.dll"
#r "../../packages/VDS.Common/lib/net40-client/VDS.Common.dll"
#r "../../bin/FSharp.RDF.dll"

open FSharp.RDF
open System.IO
open Swensen.Unquote
open FSharp.RDF.Ontology
open resource

open Assertion

open rdf

let s = ""
let sb = new System.Text.StringBuilder(s)

let og = Graph.empty (!!"http://sometest/ns#") [("base",!!"http://sometest/ns#")]
let r =
    resource !!"base:id"
      [ a !!"base:Type"
        objectProperty !!"base:someObjectProperty" !!"base:SomeOtherId"
        objectProperty !!"base:someNonQname" !!"http://google.com/stuff"
        dataProperty !!"base:someDataProperty" ("value"^^xsd.string)

        blank !!"base:someBlankProperty"
          [ a !!"base:BankType"
            dataProperty !!"base:someDataProperty" ("value2"^^xsd.string) ]

        one !!"base:someOtherObjectProperty" !!"base:id2"
          [ a !!"base:LinkedType"
            dataProperty !!"base:someDataProperty" ("value3"^^xsd.string) ]
        dataProperty !!"base:xmlstuff" ("<test>value</test>"^^xsd.xmlliteral)
        dataProperty !!"base:intstuff" (0^^xsd.integer)]

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
          base:xmlstuff "<test>value</test>"^^<rdf:XMLLiteral>;
          base:someNonQname <http://google.com/stuff>;
          rdf:type base:Type.
base:id2 base:someDataProperty "value3"^^xsd:string;
           rdf:type base:LinkedType.
""")

Diff.equal (Graph.diff g g')
