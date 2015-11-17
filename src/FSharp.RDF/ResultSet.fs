namespace FSharp.RDF

module Store =
  open VDS.RDF
  open VDS.RDF.Query
  open VDS.RDF.Query.Datasets
  open VDS.RDF.Storage
  open VDS.RDF.Storage.Management
  open VDS.RDF.Storage.Management.Provisioning.Stardog
  open VDS.RDF.Parsing

  let private parser (s:string) = SparqlQueryParser().ParseFromString s

  type Param =
    | Literal of FSharp.RDF.Literal
    | Uri of FSharp.RDF.Uri

  type ParametisedSparql =
    | ParametisedSparql of string * (string * Param) seq

  type PreparedSparql =
    | PreparedSparql of SparqlParameterizedString

  let private query f q px =
      let qs = new SparqlParameterizedString(CommandText = q)
      f qs.Namespaces
      for (k, p) in px do
        match p with
        | Param.Literal(Literal.String x)-> qs.SetLiteral(k, x)
        | Param.Uri uri -> qs.SetUri(k, System.Uri(string uri))
      PreparedSparql qs

  type TripleStoreConnection =
    | Stardog of IUpdateableStorage
    with static member queryProcessor = function
      | Stardog x -> x.Query

  type ResultSet =
   | ResultSet of SparqlResultSet
   with
    static member singles = function
      | ResultSet rx -> rx |> Seq.map (fun r -> Node.from r.[0])
    static member doubles = function
      | ResultSet rx ->
        rx |> Seq.map (fun r -> (Node.from r.[0], Node.from r.[1]))
    static member triples = function
      | ResultSet rx ->
         rx |> Seq.map (fun r -> (Node.from r.[0], Node.from r.[1], Node.from r.[2]))


  type Store =
    | Memory of FSharp.RDF.Graph
    | Remote of TripleStoreConnection
  with
    member private __.queryI (PreparedSparql q) =
      match __ with
      | Memory(FSharp.RDF.Graph g) ->
        let p = LeviathanQueryProcessor(InMemoryDataset g)
        p.ProcessQuery(parser (string q))
      | Remote x -> (TripleStoreConnection.queryProcessor x)(string q)

    member private __.query ns q px =
     let f (m:INamespaceMapper) =
        for (k,v) in ns do m.AddNamespace(k,v |> Uri.toSys)
     __.queryI (query f q px)

    member __.queryGraph ns q px =
       __.query ns q px :?> VDS.RDF.IGraph |> Graph.Graph

    member __.queryResultSet ns q px =
       __.query ns q px :?> SparqlResultSet |> ResultSet

    static member memoryFrom = Graph.loadFrom >> Memory
    static member memoryFromTTL = Graph.loadTtl >> Memory
    static member stardog u s user pass reasoning =
      let reasoningMode () =
        match reasoning with
          | true -> StardogReasoningMode.SL
          | false -> StardogReasoningMode.None

      let s = new VDS.RDF.Storage.StardogV2Connector(u,s,reasoningMode(),user,pass)
      Remote(Stardog s)


  let private defaultUri = null :> System.Uri


