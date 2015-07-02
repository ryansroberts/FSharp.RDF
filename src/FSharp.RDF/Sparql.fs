namespace FSharp.RDF

module Sparql =
  type Binding =
    | Binding of string
    | Wildcard

  type QueryType =
    | Select of Binding list

  type QueryPattern =
    | Binding of Binding
    | OfType
    | Node of Node
    static member uri u = QueryPattern.Node(u)
    static member var v = QueryPattern.Binding(Binding.Binding v)

  type BGP =
    | BGP of QueryPattern * QueryPattern * QueryPattern
    static member a b t =
      BGP(QueryPattern.var b, QueryPattern.OfType, QueryPattern.uri t)
    static member anIndividual b =
      BGP.a b (Node.Uri(Uri.from ("http://www.w3.org/2002/07/owl#Individual")))
    static member anyStatement s p o =
      BGP
        (QueryPattern.Binding(Binding.Binding s),
         QueryPattern.Binding(Binding.Binding p),
         QueryPattern.Binding(Binding.Binding o))
