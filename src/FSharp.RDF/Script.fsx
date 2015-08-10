#r "../../packages/dotNetRDF/lib/net40/dotNetRDF.dll"
#r "../../packages/VDS.Common/lib/net40-client/VDS.Common.dll"
#r "../../packages/FSharpx.Core/lib/40/FSharpx.Core.dll"
#r "../../packages/Unquote/lib/net40/Unquote.dll"
#r "../../packages/json-ld.net/lib/net40-Client/JsonLD.dll"
#I "../../packages/Newtonsoft.Json/lib/net40/"
#r "../../packages/HtmlAgilityPack/lib/Net40/HtmlAgilityPack.dll"
#r "../../packages/Newtonsoft.Json/lib/net40/Newtonsoft.Json.dll"
#r "../../lib/owlapi.net_release_1_0_0/owlapi.dll"
#r "../../lib/owlapi.net_release_1_0_0/IKVM.OpenJDK.Core.dll"
#r "../../lib/owlapi.net_release_1_0_0/Cognitum.OwlApi.Net.ReasonerInterface.dll"
#r "../../lib/owlapi.net_release_1_0_0/Reasoners/Pellet/Cognitum.OwlApi.Net.Pellet.dll"
#I "../../lib/owlapi.net_release_1_0_0/"
#r "../../bin/FSharp.RDF.dll"


open FSharp.RDF
open System.IO
open Swensen.Unquote
open Store
open FSharp.RDF.Provider

[<Literal>]
let pizzaF = __SOURCE_DIRECTORY__ + "/Pizza.ttl"
type pizzas = FSharp.RDF.OntologyProvider<pizzaF,"owl:Thing">
