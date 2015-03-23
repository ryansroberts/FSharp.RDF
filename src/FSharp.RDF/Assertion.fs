namespace FSharp.RDF
module Assertion =
  open VDS.RDF
  open VDS.RDF.Writing
  open VDS.RDF.Writing.Formatting
  open FSharpx
  open Store


  type Output =
    | Graph of VDS.RDF.IGraph
  with static member toGraph (baseUri) =
    let g = new VDS.RDF.Graph()
    addPrefixes (g,baseUri)
    Graph g

  let asrt (R ((S s),xst)) (Output.Graph g) =
    let toVDSNode n : INode =
      match n with
      | (Literal.String s) -> s.ToLiteral g  :> INode
      | (Literal.DateTimeOffset d) -> d.ToLiteral g  :> INode
    let rec asrtSt (s:INode) xst =
        for st in xst do
        match st with
            | (P p,O (Node.Blank (Blank.Blank (xst')),_)) ->
                let b = g.CreateBlankNode()
                g.Assert(Triple(g.CreateUriNode (string s),g.CreateUriNode(string p),b)) |> ignore
                asrtSt b xst'.Value
            | (P p,O (Node.Uri o,xr)) ->
                let o = g.CreateUriNode (string o)
                g.Assert(Triple(g.CreateUriNode (string s),g.CreateUriNode(string p),o)) |> ignore
                for R(s,xst') in xr.Value do
                  asrtSt o xst'
            | (P p,O (Node.Literal l,_)) ->
                g.Assert(Triple(g.CreateUriNode(string s),g.CreateUriNode(string p),toVDSNode l)) |> ignore
    let s = g.CreateUriNode (string s)
    asrtSt s xst

  let (>>>) = asrt
  module xsd =
    let string s = Node.Literal (Literal.String s)
    let datetime d = Node.Literal (Literal.DateTimeOffset d)


  let suri u =(Uri.Sys u)
  let uri u = (Uri.Sys (System.Uri u))
  let objectProperty p o = ((P p),O(Node.Uri o,lazy []))
  let one p o xst = ((P p),O(Node.Uri o,lazy
                             [for (P p',O(o',xst' )) in xst -> R(S o,(P p',O (o',xst')))]
                             ))
  let a t = objectProperty (uri "rdf:type") t
  let dataProperty p o = ((P p),O(o,lazy[]))
  let blank p xst = (P p,O(Node.Blank (Blank.Blank (lazy xst)),lazy []))
  let resource s xst = R(S s,xst)

  let (!) = uri
  let inline (^^^) t f = f t

  resource !"base:id" [
    a !"base:Type"
    objectProperty !"base:someObjectProperty" !"base:SomeOtherId"
    dataProperty !"base:someDataProperty" ("value"^^^xsd.string)
    blank !"base:someBlankProperty" [
      a !"base:BankType"
      dataProperty !"base:someDataProperty" ("value2"^^^xsd.string)
      ]
    one !"base:someOtherObjectProperty" !"base:id2" [
      a !"base:LinkedType"
      dataProperty !"base:someDataProperty" ("value3"^^^xsd.string)
      ]
  ]
  >>> Output.toGraph "http://sometest/ns#"
