namespace FSharp.RDF

open VDS.RDF
open VDS.RDF.Writing
open VDS.RDF.Parsing
open VDS.RDF.Nodes
open FSharpx
open System.Text.RegularExpressions


[<CustomEquality; NoComparison>]
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

  static member from s = Uri.Sys(System.Uri(s))
  static member from s = Uri.Sys s
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
       | :? StringNode as s -> s.AsString () |> Literal.String
       | :? DateTimeNode as d -> d.AsDateTimeOffset () |> Literal.DateTimeOffset
       | _ -> (string) n  |> Literal.String)
       |> Node.Literal
    | :? IBlankNode as n ->
      Node.Blank( Blank.Blank (
        lazy
            n.Graph.GetTriplesWithSubject(n)
            |> Seq.map (function t -> (P (Uri.from (t.Predicate :?> IUriNode).Uri),Object.from t.Object))
            |> Seq.toList
      ))
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
  static member from u = O(Node.Uri(Uri.Sys u),lazy [])
  static member from (u:INode) =
    let n = Node.from u
    match n with
      | Node.Blank (Blank.Blank (xs)) -> O(n, lazy [R(Subject.from "http://anon",xs.Value)]) //System.Uri will choke if the scheme is blank, so hack
      | Node.Uri uri ->
        O(n,lazy
          let uri = u.Graph.CreateUriNode(string uri)
          u.Graph.GetTriplesWithSubject uri |> Resource.from
          )
      | n -> O(n,lazy [])

and Statement = (Predicate * Object)

and Triple = Subject * Predicate * Object

and Resource =
  | R of Subject * Statement list
  static member from (xt:VDS.RDF.Triple seq) =
    xt
    |> Seq.groupBy (fun t -> t.Subject :?> IUriNode)
    |> Seq.map
         (fun (s, tx) ->
         R((S(Uri.Sys s.Uri)),
           [ for t in tx ->
             (P(Uri.Sys ( t.Predicate :?> IUriNode ).Uri), Object.from t.Object) ]))
    |> Seq.toList


and Blank =
  | Blank of Lazy<Statement list>

type Graph =
  | Graph of IGraph
  static member from (s : string) =
    let g = new VDS.RDF.Graph()
    let p = TurtleParser()
    use sr = new System.IO.StringReader(s)
    p.Load(g, sr)
    Graph g

module triple =
  let uriNode u (g : IGraph) = g.GetUriNode(u |> Uri.toSys)
  let bySubject u (g : IGraph) = g.GetTriplesWithSubject(uriNode u g)
  let byObject u (g : IGraph) = g.GetTriplesWithObject(uriNode u g)
  let byPredicate u (g : IGraph) = g.GetTriplesWithPredicate(uriNode u g)
  let byPredicateObject p o (g : IGraph) =
    g.GetTriplesWithPredicateObject(uriNode p g, uriNode o g)
  let bySubjectObject p o (g : IGraph) =
    g.GetTriplesWithSubjectObject(uriNode p g, uriNode o g)
  let bySubjectPredicate p o (g : IGraph) =
    g.GetTriplesWithSubjectPredicate(uriNode p g, uriNode o g)


  let fromSingle (f : Uri -> IGraph -> VDS.RDF.Triple seq) x (Graph g) =
    f x g |> Resource.from
  let fromDouble (f : Uri -> Uri -> IGraph -> VDS.RDF.Triple seq) x y (Graph g) =
    f x y g |> Resource.from

module prefixes =
  let prov = "http://www.w3.org/ns/prov#"
  let rdf = "http://www.w3.org/1999/02/22-rdf-syntax-ns#"
  let owl = "http://www.w3.org/2002/07/owl#"
  let cnt = "http://www.w3.org/2011/content#"
  let compilation = "http://nice.org.uk/ns/compilation#"
  let git2prov = "http://nice.org.uk/ns/prov/"

module uri =
  open prefixes

  let a = Uri.from (rdf + "type")

module resource =
  open triple
  open prefixes

  let fromSubject u g = fromSingle bySubject u g
  let fromPredicate u g = fromSingle byPredicate u g
  let fromObject u g = fromSingle byObject u g
  let fromPredicateObject x y g = fromDouble byPredicateObject x y g
  let fromSubjectObject x y g = fromDouble bySubjectObject x y g
  let fromSubjectPredicate x y g = fromDouble bySubjectPredicate x y g

  let asTriples (s, px) =
    seq {
      for (p, o) in px -> (s, p, o)
    }

  let mapObject f (O (o,_)) = f o

  let mapO f = List.map (mapObject f)

  let id (R(S s,_)) = s

  let traverse xo =
    [ for (O (_,next)) in xo do yield! next.Value]

  let (|Is|_|) u (R(S s, _)) =
    match u = s with
      | true -> Some u
      | _ -> None

  let noneIfEmpty =
    function
    | [] -> None
    | x :: xs -> Some(x :: xs)

  let (|Property|_|) p (R(_, xs)) =
    xs
    |> Seq.filter (fun ((P p'), _) -> p = p')
    |> Seq.map (fun (_, o) -> o)
    |> Seq.toList
    |> noneIfEmpty

  let (|FunctionalProperty|_|) p (R(_, xs)) =
    xs
    |> Seq.filter (fun ((P p'), _) -> p = p')
    |> Seq.map (fun (_, o) -> o)
    |> Seq.toList
    |> (function
    | x :: xs -> Some x
    | _ -> None)

  let (|DataProperty|_|) p f r =
    match r with
    | Property p xo -> Some(mapO f xo)

  let (|FunctionalDataProperty|_|) p f r =
    match r with
    | FunctionalProperty p (O (o,_)) -> Some(f o)

  let (|Traverse|_|) p r =
    match r with
    | Property p xo -> traverse xo |> noneIfEmpty

  let (|TraverseFunctional|_|) p r =
    match r with
    | Property p xo -> traverse xo |> (function | x::xs -> Some x | _ -> None)

  let (|HasType|_|) t (R(_, xs)) =
    xs
    |> Seq.filter
         (fun ((P p), (O(Uri o,_))) ->
         p = Uri.from (prefixes.rdf + "type") && t = o)
    |> Seq.map (fun (_, (O(Uri o,_))) -> o)
    |> Seq.toList
    |> noneIfEmpty

module xsd =
  open VDS.RDF

  let mapL f n =
    match n with
    | Node.Literal l -> (f l)
    | _ -> failwith (sprintf "%A is not a literal node" n)

  let string = function | Node.Literal ( Literal.String s ) -> s | n -> (string) n
  let datetimeoffset = function | Node.Literal ( Literal.DateTimeOffset d ) -> d | n -> failwith "%A is not a datetime node"
