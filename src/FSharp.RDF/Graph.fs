namespace FSharp.RDF

open VDS.RDF
open VDS.RDF.Writing
open VDS.RDF.Writing.Formatting
open VDS.RDF.Parsing
open VDS.RDF.Nodes
open FSharpx
open System
open System.Text.RegularExpressions

[<CustomEquality;CustomComparison>]
type Uri =
  | Sys of System.Uri
  | VDS of System.Uri * (System.Uri -> bool)

  override x.ToString() =
    match x with
    | Sys uri -> string uri

  override u.Equals(u') =
    match u' with
    | :? Uri as u' ->
      match u, u' with
      | VDS (u,_),VDS (_,u') -> string u = string u'
      | Sys u, Sys u' -> string u = string u'
      | Sys u, VDS (_,f) -> f u
      | VDS (_,f),Sys u -> f u
    | _ -> false

  override u.GetHashCode () =
    let (Sys uri) = u
    u.GetHashCode()

  interface IComparable<Uri> with
        member u.CompareTo (u') = (string u).CompareTo(string u')
  interface IComparable with
        member u.CompareTo obj =
            match obj with
            | :? Uri as u' -> (u:> IComparable<_>).CompareTo u'
            | _ -> invalidArg "obj" "not a Uri"
  interface IEquatable<Uri> with
        member u.Equals (u') = string u = string u'

  static member from s = Uri.Sys(System.Uri(s))
  static member from s = Uri.Sys s
  static member from (n:IUriNode) = (Uri.VDS (n.Uri,fun u -> (n.Equals(n.Graph.CreateUriNode u))))
  static member toSys (x : Uri) = System.Uri(string x)

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
       | :? StringNode as s -> s.AsString() |> Literal.String
       | :? DateNode as d -> d.AsDateTimeOffset() |> Literal.DateTimeOffset
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
      Node.Blank(Blank.Blank(lazy traverseBlank()))
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
  | DateTimeOffset of System.DateTimeOffset

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
      O(n, lazy [ R(Subject.from "http://anon", xs.Value) ]) //System.Uri will choke if the scheme is blank, so hack
    | Node.Uri(Sys uri) ->
      O(n,
        lazy let uri = u.Graph.CreateUriNode(uri)
             u.Graph.GetTriplesWithSubject uri |> Resource.from)
    | n -> O(n, lazy [])

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
  | Blank of Lazy<Statement list>
  with override x.ToString() =
    match x with
      | Blank xs -> sprintf "%A" (xs.Value)

type Graph =
  | Graph of IGraph


module prefixes =
  let prov = "http://www.w3.org/ns/prov#"
  let rdf = "http://www.w3.org/1999/02/22-rdf-syntax-ns#"
  let owl = "http://www.w3.org/2002/07/owl#"
  let cnt = "http://www.w3.org/2011/content#"
  let compilation = "http://nice.org.uk/ns/compilation#"
  let git2prov = "http://nice.org.uk/ns/prov/"
module wellknown =
  open prefixes
  let rdftype = rdf + "type" |> Uri.from


[<AutoOpen>]
module graph =
   open prefixes
   let toString (s : System.Text.StringBuilder) = new System.IO.StringWriter(s) :> System.IO.TextWriter
   let toFile (p) = new System.IO.StreamWriter ( System.IO.File.OpenWrite p  )

   module private parse =
      let ttl () = new TurtleParser() :> IRdfReader
   module private formatWrite =
      let ttl () = CompressingTurtleWriter() :> IRdfWriter
   module private formatStream =
      let ttl (Graph g) = TurtleFormatter() :> ITripleFormatter

   let private load (f:IRdfReader) (sr : System.IO.TextReader) =
        let g = new VDS.RDF.Graph()
        f.Load(g, sr)
        Graph g

   let private write (f:IRdfWriter) (tw : System.IO.TextWriter) (Graph g) =
        f.Save(g, tw)

   let private stream (f:ITripleFormatter) (tw : System.IO.TextWriter) tx = seq {
        for t in tx do
          f.Format(t) |> tw.WriteLine
          yield t
        tw.Close()
      }

   type Graph with
      static member loadFrom (s : string) =
        let g = new VDS.RDF.Graph()
        match s.StartsWith("http") with
        | true -> g.LoadFromUri(System.Uri s)
        | _ -> g.LoadFromFile s
        Graph g

      static member fromString (s:string) = new System.IO.StringReader(s) :> System.IO.TextReader


      static member addPrefixes (Sys baseUri) xp (Graph g) =
        g.BaseUri <- baseUri
        ("base",(Sys baseUri))::xp
        |> List.iter
                (fun (p, (Sys ns)) -> g.NamespaceMap.AddNamespace(p, ns))

      static member defaultPrefixes baseUri xp g =
        Graph.addPrefixes baseUri ([("prov", Uri.from prov)
                                    ("rdf", Uri.from rdf)
                                    ("owl", Uri.from owl)
                                    ("git2prov", Uri.from git2prov)
                                    ("compilation", Uri.from compilation)
                                    ("cnt", Uri.from cnt) ] @ xp) g

      static member diff (Graph g) (Graph g') = g.Difference g'

      static member empty baseUri xp =
        let g = (Graph ( new VDS.RDF.Graph() ))
        Graph.defaultPrefixes baseUri xp g
        g

      static member streamTtl g = stream (formatStream.ttl g)
      static member writeTtl = write (formatWrite.ttl ())
      static member loadTtl = load (parse.ttl ())

module triple =
  let uriNode (Sys u) (Graph g) = g.CreateUriNode(u)
  let bySubject u (Graph g) = g.GetTriplesWithSubject(uriNode u (Graph g))
  let byObject u (Graph g) = g.GetTriplesWithObject(uriNode u (Graph g))
  let byPredicate u (Graph g) = g.GetTriplesWithPredicate(uriNode u (Graph g))
  let byPredicateObject p o (Graph g) =
    g.GetTriplesWithPredicateObject(uriNode p (Graph g), uriNode o (Graph g))
  let bySubjectObject p o (Graph g) =
    g.GetTriplesWithSubjectObject(uriNode p (Graph g), uriNode o (Graph g))
  let bySubjectPredicate p o (Graph g) =
    g.GetTriplesWithSubjectPredicate(uriNode p (Graph g), uriNode o (Graph g))
  let byType o (Graph g) = seq {
    for s in g.GetTriplesWithPredicateObject(uriNode (Uri.from "http://www.w3.org/1999/02/22-rdf-syntax-ns#type") (Graph g), uriNode o (Graph g)) do
      yield! g.GetTriplesWithSubject(s.Subject);
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
    static member fromSubject  = fromSingle bySubject
    static member fromPredicate  = fromSingle byPredicate
    static member fromObject  = fromSingle byObject
    static member fromPredicateObject = fromDouble byPredicateObject
    static member fromSubjectObject  = fromDouble bySubjectObject
    static member fromSubjectPredicate  = fromDouble bySubjectPredicate
    static member fromType = fromSingle byType
    static member id (R(S s, _)) = s
    static member asTriples (R(s,px) ) = seq {
      for (p, o) in px -> (s, p, o)
    }

  let traverse xo =
    [ for (O(_, next)) in xo do
        yield! next.Value ]

  let (|Is|_|) u (R(S s, _)) =
    match u = s with
    | true -> Some u
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

  let (|ObjectProperty|_|) p = function
    | Property p x ->
      x |> List.map (function
                   | O(Uri u,_) -> Some u
                   | _ -> None)
      |> List.filter Option.isSome
      |> List.map Option.get
      |> noneIfEmpty
    | _ -> None

  let private listOfOne = function
    | x::_ -> Some x
    | _ -> None

  let (|FunctionalProperty|_|) p = function
    | Property p x -> listOfOne x
    | _ -> None

  let (|FunctionalObjectProperty|_|) p = function
    | ObjectProperty p x -> listOfOne x
    | _ -> None

  let (|DataProperty|_|) p f = function
    | Property p xo -> Some(mapO f xo)
    | _ -> None

  let (|FunctionalDataProperty|_|) p f = function
    | FunctionalProperty p (O(o, _)) -> Some(f o)
    | _ -> None

  let (|Traverse|_|) p = function
    | Property p xo -> traverse xo |> noneIfEmpty
    | _ -> None

  let (|TraverseFunctional|_|) p = function
    | Traverse p xo -> listOfOne xo
    | _ -> None

  let (|HasType|_|) t = function
    | ObjectProperty wellknown.rdftype xs -> List.filter ((=) t) xs |> listOfOne
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
