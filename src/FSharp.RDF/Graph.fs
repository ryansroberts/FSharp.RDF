namespace FSharp.RDF

open VDS.RDF
open VDS.RDF.Writing
open VDS.RDF.Writing.Formatting
open VDS.RDF.Parsing
open VDS.RDF.Nodes
open FSharpx
open System
open System.Text.RegularExpressions
open VDS.RDF.Ontology

[<CustomEquality; CustomComparison>]
type Uri =
  | Sys of System.Uri

  override x.ToString() =
    match x with
    | Sys uri -> string uri

  override u.Equals(u') =
    match u' with
    | :? Uri as u' ->
      match u, u' with
      | Sys u, Sys u' -> string u = string u'
    | _ -> false

  override x.GetHashCode() =
    match x with
    | Sys u -> u.GetHashCode()

  interface IComparable<Uri> with
    member u.CompareTo(u') = (string u).CompareTo(string u')

  interface IComparable with
    member u.CompareTo obj =
      match obj with
      | :? Uri as u' -> (u :> IComparable<_>).CompareTo u'
      | _ -> invalidArg "obj" "not a Uri"

  interface IEquatable<Uri> with
    member u.Equals(u') = string u = string u'

  static member from s = Uri.Sys(System.Uri(s))
  static member from s = Uri.Sys s
  static member toSys (Uri.Sys u) = u

[<CustomEquality; NoComparison>]
type Node =
  | Uri of Uri
  | Blank of Blank
  | Literal of Literal

  static member from (n : INode) =
    match n with
    | :? IUriNode as n -> Node.Uri(Uri.Sys n.Uri)
    | :? ILiteralNode as n ->
      (match n with
       | :? StringNode as s ->
         match s.DataType with
         | null -> s.AsString() |> Literal.String
         | dt when dt.AbsoluteUri = "rdf.xmlLiteral" -> s.AsString() |> Literal.XMLLiteral
         | dt -> failwith (dt.AbsoluteUri + " not supported")
       | :? DateNode as d -> d.AsDateTimeOffset() |> Literal.DateTimeOffset
       | :? NumericNode as i -> i.AsInteger() |> Literal.Integer
       | :? DateTimeNode as d -> d.AsDateTimeOffset() |> Literal.DateTimeOffset
       | _ -> n.Value |> Literal.String)
      |> Node.Literal
    | :? IBlankNode as n ->
      let traverseBlank() =
        n.Graph.GetTriplesWithSubject n
        |> Seq.map
             (function
             | t ->
               (P(Uri.from (t.Predicate :?> IUriNode).Uri), Object.from t.Object))
        |> Seq.toList
      Node.Blank(Blank.Blank(traverseBlank()))
    | _ -> failwith (sprintf "Unknown node %A" (n.GetType()))

  static member from (u : string) = Node.Uri(Uri.from u)
  static member from (u : System.Uri) = Node.Uri(Uri.from u)
  override n.Equals(n') =
    match n' with
    | :? Node as n' ->
      match n, n' with
      | Uri n, Uri n' -> n = n'
      | _ -> false
    | _ -> false

and Literal =
  | String of string
  | Integer of System.Int64
  | DateTimeOffset of System.DateTimeOffset
  | XMLLiteral of string

and Subject =
  | S of Uri
  static member from (u : string) = S(Uri.from u)

and Predicate =
  | P of Uri
  static member from u = P(Uri.Sys u)
  static member from u = P(Uri.Sys(System.Uri u))

and Object =
  | O of Node * Lazy<Resource list>
  static member from u = O(Node.Uri(Uri.Sys u), lazy [])

  static member from (u : INode) =
    let n = Node.from u
    match n with
    | Node.Blank(Blank.Blank(xs)) ->
      O(n, lazy [ R(Subject.from "http://anon", xs) ]) //System.Uri will choke if the scheme is blank, so hack
    | Node.Uri(Sys uri) ->
      O(n,
        lazy let uri = u.Graph.CreateUriNode(uri)
             u.Graph.GetTriplesWithSubject uri |> Resource.from)
    | n -> O(n, lazy [])

  override __.ToString() =
    match __ with
    | O(_, xs) -> sprintf "%A" xs.Value

and Statement = Predicate * Object

and Triple = Subject * Predicate * Object

and Resource =
  | R of Subject * Statement list
  static member from (xt : VDS.RDF.Triple seq) =
    xt
    |> Seq.groupBy (fun t -> t.Subject :?> IUriNode)
    |> Seq.map
         (fun (s, tx) ->
         R
           ((S(Uri.Sys s.Uri)),
            [ for t in tx ->
                (P(Uri.Sys (t.Predicate :?> IUriNode).Uri), Object.from t.Object) ]))
    |> Seq.toList

and Blank =
  | Blank of Statement list

type Graph =
  | Graph of IGraph

type Diff =
  | Diff of VDS.RDF.GraphDiffReport
  with static member equal (Diff x) = x.AreEqual
       override x.ToString() =
         String.Join("\n",
                [ let (Diff report) = x
                  let formatter = NTriplesFormatter()
                  if (report.AreEqual) then
                    yield ("Graphs are Equal")
                    yield ""
                    yield ("Blank Node Mapping between Graphs:")
                    for kvp in report.Mapping do
                      yield (kvp.Key.ToString(formatter) + " => "
                             + kvp.Value.ToString(formatter))
                  else
                    yield ("Graphs are non-equal")
                    yield ""
                    yield ("Triples added to 1st Graph to give 2nd Graph:")
                    for t in report.AddedTriples do
                      yield (t.ToString(formatter))
                    yield ""
                    yield ("Triples removed from 1st Graph to given 2nd Graph:")
                    for t in report.RemovedTriples do
                      yield (t.ToString(formatter))
                    yield ""
                    yield ("Blank Node Mapping between Graphs:")
                    for kvp in report.Mapping do
                      yield ((kvp.Key.ToString(formatter)) + " => "
                             + (kvp.Value.ToString(formatter)))
                    yield ""
                    yield ("MSGs added to 1st Graph to give 2nd Graph:")
                    for msg in report.AddedMSGs do
                      for t in msg.Triples do
                        yield (t.ToString(formatter))
                      yield ""
                    yield ""
                    yield ("MSGs removed from 1st Graph to give 2nd Graph:")
                    for msg in report.RemovedMSGs do
                      for t in msg.Triples do
                        yield (t.ToString(formatter))
                      yield "" ])


module prefixes =
  let rdf = "http://www.w3.org/1999/02/22-rdf-syntax-ns#"
  let owl = "http://www.w3.org/2002/07/owl#"

module wellknown =
  open prefixes

  let rdftype = rdf + "type" |> Uri.from

[<AutoOpen>]
module graph =
  let private hasSubject (S s) (P p, O(o, xs)) = (S s, P p, O(o, xs))
  let private hasPredicate (P p) (O(o, xs)) = (P p, O(o, xs))

  open prefixes

  let toString (s : System.Text.StringBuilder) =
    new System.IO.StringWriter(s) :> System.IO.TextWriter
  let toFile (p) =
    new System.IO.StreamWriter(System.IO.File.OpenWrite p) :> System.IO.TextWriter
  let appendFile (p) = System.IO.File.AppendText p :> System.IO.TextWriter
  let toStream (s : System.IO.Stream) =
    new System.IO.StreamWriter(s) :> System.IO.TextWriter
  let fromString (s : string) =
    new System.IO.StringReader(s) :> System.IO.TextReader
  let fromStream (s : System.IO.Stream) =
    new System.IO.StreamReader(s) :> System.IO.TextReader

  module private parse =
    let ttl() = new TurtleParser() :> IRdfReader

  module private formatWrite =
    let ttl() = CompressingTurtleWriter() :> IRdfWriter

  module private formatStream =
    let ttl (Graph g) = TurtleFormatter() :> ITripleFormatter

  let private load (f : IRdfReader) (sr : System.IO.TextReader) =
    let g = new VDS.RDF.Graph()
    f.Load(g, sr)
    Graph g

  let private write (f : IRdfWriter) (tw : System.IO.TextWriter) (Graph g) =
    f.Save(g, tw)

  let private stream (f : ITripleFormatter) (tw : System.IO.TextWriter) tx =
    seq {
      for t in tx do
        f.Format t |> tw.WriteLine
        yield t
    }

  type Graph with

    static member loadFrom (s : string) =
      let g = new VDS.RDF.Graph()
      match s.StartsWith("http") with
      | true -> g.LoadFromUri(System.Uri s)
      | _ -> g.LoadFromFile s
      Graph g

    static member addPrefixes (baseUri) xp (Graph g) =
      g.BaseUri <- Uri.toSys baseUri
      ("base", baseUri) :: xp
      |> List.iter (fun (p, (Uri.Sys ns)) -> g.NamespaceMap.AddNamespace(p, ns))
      Graph g

    static member defaultPrefixes baseUri xp g =
      Graph.addPrefixes baseUri ([ ("rdf", Uri.from rdf)
                                   ("owl", Uri.from owl) ]
                                 @ xp) g

    static member diff (Graph g) (Graph g') = g.Difference g' |> Diff

    static member print (Graph g) =
      let s = System.Text.StringBuilder()
      let w = new VDS.RDF.Writing.CompressingTurtleWriter()
      use sw = new System.IO.StringWriter(s)
      w.Save(g, sw)
      s.ToString()

    static member merge (Graph g) (Graph g') =
      g.Merge(g')
      Graph g

    static member empty baseUri xp =
      let g = (Graph(new VDS.RDF.Graph()))
      Graph.defaultPrefixes baseUri xp g

    static member unnamed xp =
      let (Graph g) = Graph.empty (Uri.from "http://mutable") xp
      g.BaseUri <- (null :> System.Uri)
      Graph g

    static member threadSafe (Graph g) =
      let g' = new ThreadSafeGraph(g.Triples)
      g'.BaseUri <- g.BaseUri
      for p in g.NamespaceMap.Prefixes do
        g'.NamespaceMap.AddNamespace(p, g.NamespaceMap.GetNamespaceUri p)
      Graph g'

    static member streamTtl g = stream (formatStream.ttl g)
    static member writeTtl = write (formatWrite.ttl())
    static member loadTtl = load (parse.ttl())

module private triple =
  let uriNode u (Graph g) = g.CreateUriNode(Uri.toSys u)
  let private rdfType =
    Uri.from "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"
  let bySubject u (Graph g) = g.GetTriplesWithSubject(uriNode u (Graph g))
  let byObject u (Graph g) = g.GetTriplesWithObject(uriNode u (Graph g))
  let byPredicate u (Graph g) = g.GetTriplesWithPredicate(uriNode u (Graph g))
  let byPredicateObject p o (Graph g) =
    g.GetTriplesWithPredicateObject(uriNode p (Graph g), uriNode o (Graph g))
  let bySubjectObject p o (Graph g) =
    g.GetTriplesWithSubjectObject(uriNode p (Graph g), uriNode o (Graph g))
  let bySubjectPredicate p o (Graph g) =
    g.GetTriplesWithSubjectPredicate(uriNode p (Graph g), uriNode o (Graph g))

  let byType o (Graph g) =
    seq {
      for s in g.GetTriplesWithPredicateObject
                 (uriNode rdfType (Graph g), uriNode o (Graph g)) do
        yield! g.GetTriplesWithSubject(s.Subject)
    }

  let fromSingle f x g = f x g |> Resource.from
  let fromDouble f x y g = f x y g |> Resource.from

[<AutoOpen>]
module resource =
  open triple
  open prefixes

  let mapObject f (O(o, _)) = f o
  let mapO f = List.map (mapObject f)

  type Resource with
    static member fromSubject = fromSingle bySubject
    static member fromPredicate = fromSingle byPredicate
    static member fromObject = fromSingle byObject
    static member fromPredicateObject = fromDouble byPredicateObject
    static member fromSubjectObject = fromDouble bySubjectObject
    static member fromSubjectPredicate = fromDouble bySubjectPredicate
    static member fromType = fromSingle byType
    static member id (R(S s, _)) = s
    static member asTriples (R(s, px)) =
      seq {
        for (p, o) in px -> (s, p, o)
      }

  let traverse xo =
    [ for (O(_, next)) in xo do
        yield! next.Value ]

  let (|Is|_|) u (R(S s, _)) =
    match u = s with
    | true -> Some Is
    | _ -> None

  let private noneIfEmpty =
    function
    | [] -> None
    | x :: xs -> Some(x :: xs)

  let (|Property|_|) p (R(_, xs)) =
    xs
    |> Seq.filter (fun ((P p'), _) -> p = p')
    |> Seq.map (fun (_, o) -> o)
    |> Seq.toList
    |> noneIfEmpty

  let (|ObjectProperty|_|) p =
    function
    | Property p x ->
      x
      |> List.map (function
           | O(Uri u, _) -> Some u
           | _ -> None)
      |> List.filter Option.isSome
      |> List.map Option.get
      |> noneIfEmpty
    | _ -> None

  let private listOfOne =
    function
    | x :: _ -> Some x
    | _ -> None

  let (|FunctionalProperty|_|) p =
    function
    | Property p x -> listOfOne x
    | _ -> None

  let (|FunctionalObjectProperty|_|) p =
    function
    | ObjectProperty p x -> listOfOne x
    | _ -> None

  let (|DataProperty|_|) p f =
    function
    | Property p xo -> Some(mapO f xo)
    | _ -> None

  let (|FunctionalDataProperty|_|) p f =
    function
    | FunctionalProperty p (O(o, _)) -> Some(f o)
    | _ -> None

  let (|Traverse|_|) p =
    function
    | Property p xo -> traverse xo |> noneIfEmpty
    | _ -> None

  let (|TraverseFunctional|_|) p =
    function
    | Traverse p xo -> listOfOne xo
    | _ -> None

  let (|HasType|_|) t =
    function
    | ObjectProperty wellknown.rdftype xs ->
        if List.exists ((=) t) xs then Some HasType else None
    | _ -> None

module xsd =
  open VDS.RDF

  let string =
    function
    | Node.Literal(Literal.String s) -> s
    | n -> failwith (sprintf "%A is not a string node" n)

  let datetimeoffset =
    function
    | Node.Literal(Literal.DateTimeOffset d) -> d
    | n -> failwith "%A is not a datetime node"
