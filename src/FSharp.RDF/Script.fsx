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

let dog = Store.stardog "http://192.168.59.103:5820" "nice" "admin" "admin"

dog.queryGraph [] "CONSTRUCT {?s ?p ?o} WHERE {?s ?p ?o} LIMIT 10" []
|> Graph.print

dog.queryResultSet [] "SELECT ?s ?p ?o WHERE {?s ?p ?o} LIMIT 10" []
|> ResultSet.triples
|> List.ofSeq
