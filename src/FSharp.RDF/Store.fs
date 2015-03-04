module Store
open VDS.RDF
open VDS.RDF.Query
open VDS.RDF.Query.Datasets
open VDS.RDF.Storage
open VDS.RDF.Storage.Management
open VDS.RDF.Parsing
open Uri


let parser = SparqlQueryParser()

type store =
    | Memory of IGraph
    member x.QueryProcessor() = 
        match x with
            | Memory m -> LeviathanQueryProcessor(InMemoryDataset(m))


let defaultUri = null :> System.Uri
let loadGph (g : Graph) = store.Memory g

let loadFile (s : string) = 
    let g = new Graph()
    match s.StartsWith("http") with
    | true -> g.LoadFromUri(System.Uri s)
    | false -> g.LoadFromFile s
    Memory g

let loadTtl (s : System.IO.Stream) = 
    let g = new Graph()
    let p = new TurtleParser()
    use sr = new System.IO.StreamReader(s)
    p.Load(g, sr)
    Memory g


type Param =
    | Literal of string * string
    | Uri of string * Uri

let pquery (store:store) (q:string) px =
    match store with
        | Memory g ->
            let qs = new SparqlParameterizedString(CommandText=q,NameSpaces=g.NamespaceMap)
            for p in px do
                match k,p with
                | k,Literal l -> qs.SetLiteral (k,l)
                | k,Param.Uri uri -> qs.SetUri (k,System.Uri(string uri))
            qs.QueryProcessor <- store.QueryProcessor
            qs :> SparqlQuery

let query (store:store) q = parser.ParseFromString q :> SparqlQuery

let exec (store : store) q = 
    (store.QueryProcessor()).ProcessQuery q


let construct (store : store) q = query store q :?> IGraph
let resultset (store : store) q = ResultSet ( query store q :?> SparqlResultSet )

let defaultNamespaces s bas =
    match s with 
    | Memory g -> ns.add (g,bas)

let dump s  =
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

