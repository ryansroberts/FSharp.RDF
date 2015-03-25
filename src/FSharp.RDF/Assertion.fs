namespace FSharp.RDF

module Assertion =
  open VDS.RDF
  open VDS.RDF.Writing
  open VDS.RDF.Writing.Formatting
  open FSharpx
  open FSharp.RDF
  open Store

  module output =
    let private asrt (R((S(Uri.Sys s)), xst)) (FSharp.RDF.Graph g) =
      let toVDSNode n : INode =
        match n with
        | (Literal.String s) -> s.ToLiteral g :> INode
        | (Literal.DateTimeOffset d) -> d.ToLiteral g :> INode

      let rec asrtSt (s : INode) xst =
        for st in xst do
          match st with
          | (P(Uri.Sys p), O(Node.Blank(Blank.Blank(xst')), _)) ->
            let b = g.CreateBlankNode()
            g.Assert(Triple(s, g.CreateUriNode p, b)) |> ignore
            asrtSt b xst'.Value
          | (P(Uri.Sys p), O(Node.Uri(Sys o), xr)) ->
            let o = g.CreateUriNode o
            g.Assert(Triple(s, g.CreateUriNode p, o)) |> ignore
            for R(s, xst') in xr.Value do
              asrtSt o xst'
          | (P(Uri.Sys p), O(Node.Literal l, _)) ->
            g.Assert(Triple(s, g.CreateUriNode p, toVDSNode l)) |> ignore

      let s = g.CreateUriNode s
      asrtSt s xst

    let toGraph baseUri xp xr =
      let g = (Graph ( new VDS.RDF.Graph() ))
      graph.defaultPrefixes baseUri xp g
      for r in xr do
        asrt r g
      g

    let ttl () = CompressingTurtleWriter() :> IRdfWriter

    let format (f:unit -> IRdfWriter) (tw : System.IO.TextWriter) o =
      match o with
      | FSharp.RDF.Graph g ->
        (f()).Save(g, tw)
        o

    let toString (s : System.Text.StringBuilder) = new System.IO.StringWriter(s)

  module xsd =
    let string s = Node.Literal(Literal.String s)
    let datetime d = Node.Literal(Literal.DateTimeOffset d)

  let uri u = (Uri.Sys(System.Uri u))
  let (!) = uri
  let inline (^^) t f = f t

  module rdf =
    let objectProperty p o = ((P p), O(Node.Uri o, lazy []))
    let one p o xst = ((P p), O(Node.Uri o, lazy [ R(S o, xst) ]))
    let a t = objectProperty (uri "rdf:type") t
    let dataProperty p o = ((P p), O(o, lazy []))
    let blank p xst = (P p, O(Node.Blank(Blank.Blank(lazy xst)), lazy []))
    let resource s xst = R(S s, xst)

  module owl =
    let individual s xt xst = R(S s,
      [
        yield rdf.a !"owl:NamedIndividual"
        for t in xt -> rdf.a t
      ] @ xst)

    let cls s xt xst = R(S s,
      [
        yield rdf.a !"owl:Class"
        for t in xt -> rdf.a t
      ] @ xst)
