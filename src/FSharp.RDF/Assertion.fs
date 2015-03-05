module Assertion

open VDS.RDF
open Graph

let literal (g : IGraph) s = Literal(g.CreateLiteralNode s)
let uri (g : IGraph) u = VDS(UriFactory.Create u |> g.CreateUriNode)
let puri (g : IGraph) (u : Uri) = VDS(g.CreateUriNode(string u))
let qn (g : IGraph) (qn : string) = VDS(g.CreateUriNode qn)
let a (g : IGraph) = qn g "rdf:type"
let date (g : IGraph) (d : System.DateTimeOffset) =
  (LiteralExtensions.ToLiteral(d, g))
let triples (g : IGraph) = function
  | (s, px) ->
    px
    |> List.map (function
         | (p, o) -> Triple(s, p, o))
    |> g.Assert
    |> ignore
    ()

let blank (g : IGraph) px =
  let b = g.CreateBlankNode()
  triples g (b, px) |> ignore
  b :> INode
