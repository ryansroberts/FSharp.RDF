#r "../../packages/dotNetRDF/lib/net40/dotNetRDF.dll"
#r "../../packages/VDS.Common/lib/net40-client/VDS.Common.dll"
#r "../../packages/FSharpx.Core/lib/40/FSharpx.Core.dll"
#r "../../packages/Unquote/lib/net40/Unquote.dll"
#r "../../packages/json-ld.net/lib/net40-Client/JsonLD.dll"
#r "../../packages/Newtonsoft.Json/lib/net40/Newtonsoft.Json.dll"

#load "Graph.fs"
#load "JsonLd.fs"
#load "Store.fs"
#load "Assertion.fs"

open FSharp.RDF
open System.IO
open Swensen.Unquote
open Assertion
open rdf
open resource
open JsonLD.Core

let s = ""
let sb = new System.Text.StringBuilder(s)
let og = Graph.empty (!"http://sometest/ns#") [ ("base", !"http://sometest/ns#") ]

let r =
  resource !"base:id"
    [ a (!"base:Type")
      objectProperty !"base:someObjectProperty" !"base:SomeOtherId"
      objectProperty !"base:someOtherObjectProperty" !"https://google.com/stuff"
      dataProperty !"base:someDataProperty" ("value" ^^ xsd.string)

      blank !"base:someBlankProperty"
        [ a !"base:BankType"
          dataProperty !"base:someDataProperty" ("value2" ^^ xsd.string) ]

      one !"base:someOtherObjectProperty" !"base:id2"
        [ a !"base:LinkedType"
          dataProperty !"base:someDataProperty" ("value3" ^^ xsd.string) ] ]

let ld = Resource.toJsonLD(JsonLdOptions()) r |> Seq.head

(string ld)
