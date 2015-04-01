namespace FSharp.RDF

module Assertion =
  open VDS.RDF
  open VDS.RDF.Writing
  open VDS.RDF.Writing.Formatting
  open FSharpx
  open FSharp.RDF
  open Store

  module Assert =
    let toVDSNode g n : INode =
        match n with
        | (Literal.String s) -> s.ToLiteral g :> INode
        | (Literal.DateTimeOffset d) -> d.ToLiteral g :> INode

    let rec private assrtTriple (FSharp.RDF.Graph g) (S(Uri.Sys s),p,o) =
      let rec assrtTriple (s:INode) (p,o) =
        match p,o with
            | (P(Uri.Sys p), O(Node.Blank(Blank.Blank(xst')), _)) ->
                let b = g.CreateBlankNode()
                g.Assert(Triple(s, g.CreateUriNode p, b)) |> ignore
                for (p,o) in xst'.Value do assrtTriple b (p,o)
            | (P(Uri.Sys p), O(Node.Uri(Sys o), xr)) ->
                let o = g.CreateUriNode o
                g.Assert(Triple(s, g.CreateUriNode p, o)) |> ignore
                resources (Graph g) xr.Value |> ignore
            | (P(Uri.Sys p), O(Node.Literal l, _)) ->
                g.Assert(Triple(s, g.CreateUriNode p, toVDSNode g l)) |> ignore

      let s = g.CreateUriNode s
      assrtTriple s (p,o)


    and triples g tx =
      for t in tx do
        assrtTriple g t
      g
    and resources g xr =
      Seq.collect resource.asTriples xr
      |> triples g

    let ttl () = CompressingTurtleWriter() :> IRdfWriter

    let format (f:unit -> IRdfWriter) (tw : System.IO.TextWriter) o =
      match o with
      | FSharp.RDF.Graph g ->
        (f()).Save(g, tw)
        o


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
    let triple s (p,o) = (S s,P p,O o)

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
