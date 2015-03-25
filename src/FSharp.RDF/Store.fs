namespace FSharp.RDF

module Store =
  open VDS.RDF
  open VDS.RDF.Query
  open VDS.RDF.Query.Datasets
  open VDS.RDF.Storage
  open VDS.RDF.Storage.Management
  open VDS.RDF.Parsing


  let private parser = SparqlQueryParser()

  type Store =
    | Memory of FSharp.RDF.Graph
    member x.QueryProcessor() =
      match x with
      | Memory (FSharp.RDF.Graph g) -> LeviathanQueryProcessor(InMemoryDataset(g))

  type Query =
    | Query of SparqlParameterizedString

  type ResultSet =
    | ResultSet of SparqlResultSet

  let private defaultUri = null :> System.Uri

  type Param =
    | Literal of string
    | Uri of FSharp.RDF.Uri

  let query (store : Store) (q : string) px =
    match store with
    | Memory (FSharp.RDF.Graph g) ->
      let qs =
        new SparqlParameterizedString(CommandText = q,
                                      Namespaces = g.NamespaceMap)
      for (k, p) in px do
        match p with
        | Param.Literal l -> qs.SetLiteral(k, l)
        | Param.Uri uri -> qs.SetUri(k, System.Uri(string uri))
      qs.Namespaces <- g.NamespaceMap
      qs.QueryProcessor <- store.QueryProcessor()
      Query qs

  let resultset (store : Store) q =
    match q with
    | Query q -> q.ExecuteQuery() |> ResultSet

  let dump s =
    match s with
    | Memory (FSharp.RDF.Graph g) ->
      let s = System.Text.StringBuilder()
      let w = new VDS.RDF.Writing.CompressingTurtleWriter()
      use sw = new System.IO.StringWriter(s)
      w.Save(g, sw)
      s.ToString()

  
  open FSharp.RDF.prefixes

