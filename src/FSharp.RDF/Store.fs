module Store

open VDS.RDF
open VDS.RDF.Query
open VDS.RDF.Query.Datasets
open VDS.RDF.Storage
open VDS.RDF.Storage.Management
open VDS.RDF.Parsing
open Uri

let parser = SparqlQueryParser()

type Store = 
  | Memory of IGraph
  member x.QueryProcessor() = 
    match x with
    | Memory m -> LeviathanQueryProcessor(InMemoryDataset(m))

type Query = 
  | Query of SparqlParameterizedString

type ResultSet = 
  | ResultSet of SparqlResultSet

type ResultGraph = 
  | ResultGraph of IGraph

let defaultUri = null :> System.Uri
let loadGph (g : Graph) = Store.Memory g

let loadFile (s : string) = 
  let g = new Graph()
  match s.StartsWith("http") with
  | true -> g.LoadFromUri(System.Uri s)
  | false -> g.LoadFromFile s
  Memory g

let loadTtl (sr : System.IO.StreamReader) = 
  let g = new Graph()
  let p = new TurtleParser()
  p.Load(g, sr)
  Memory g

type Param = 
  | Literal of string
  | Uri of Uri

let query (store : Store) (q : string) px = 
  match store with
  | Memory g -> 
    let qs = 
      new SparqlParameterizedString(CommandText = q, Namespaces = g.NamespaceMap)
    for (k, p) in px do
      match p with
      | Param.Literal l -> qs.SetLiteral(k, l)
      | Param.Uri uri -> qs.SetUri(k, System.Uri(string uri))
    qs.Namespaces <- g.NamespaceMap
    qs.QueryProcessor <- store.QueryProcessor()
    Query qs

let graph (store : Store) q = ()

let resultset (store : Store) q = 
  match q with
  | Query q -> q.ExecuteQuery() |> ResultSet

let defaultNamespaces s bas = 
  match s with
  | Memory g -> ns.add (g, bas)

let dump s = 
  match s with
  | Memory g -> 
    let s = System.Text.StringBuilder()
    let w = new VDS.RDF.Writing.CompressingTurtleWriter()
    use sw = new System.IO.StringWriter(s)
    w.Save(g, sw)
    s.ToString()

let diff g g' = 
  match g, g' with
  | Memory g, Memory g' -> g.Difference g'
