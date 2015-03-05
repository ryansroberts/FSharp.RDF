module Traversal

open Graph
open FSharpx
open FSharpx.Option
open VDS.RDF
open VDS.RDF.Parsing
open System.IO

type Graph =
  | Graph of IGraph
  static member from (s : string) =
    let g = new VDS.RDF.Graph()
    let p = TurtleParser()
    use sr = new StringReader(s)
    p.Load(g, sr)
    Graph g

type Traversal<'a> =
| Traversal of (Statements list list * 'a)

let fromSubject u g =
  match g with
  | Graph g ->
    g.GetUriNode(u |> Uri.toSys)
    |> g.GetTriplesWithSubject
    |> Seq.groupBy (fun t -> t.Subject)
    |> Seq.exactlyOne //This sucks
    |> (fun (s, tx) ->
    (Subject(Node.from s)),
    [ for t in tx ->
        (Predicate(Node.from t.Predicate), Object(Node.from t.Object)) ])

let asTriples x =
  match x with
  | (s, px) ->
    [ for (p, o) in px -> (s, p, o) ]

(*Triples for Predicate p*)
let pred p (sx:Statements) : Graph.Triple seq =
  match p,sx with
  | Predicate n,(s, px) ->
    px
    |> Seq.filter (function | Predicate n', _ -> n = n')
    |> Seq.map (fun (p, o) -> (s, p, o))

let traverse p (sx : Statements) : Statements seq =
  let traverse t =
    match t with
    | (_, _, Object(Node.Uri(Uri.VDS vds))) ->
      fromSubject (Uri.VDS vds) (Graph vds.Graph) |> Option.Some
    | _ -> None
  pred p sx
  |> Seq.map traverse
  |> Seq.filter Option.isSome
  |> Seq.map Option.get

let traverseFunctional p sx = (traverse p sx) |> Seq.head
let (==>) sx p = sx |> Seq.collect (traverse p)
let (=?>) sx p c = sx ==> p |> Seq.filter c
let (.>) sx p = sx |> Seq.collect (pred p)

let mapSubject tx f =
  [ for t in tx do
      match t with
      | (Subject s, _, _) -> yield f s ]

let mapPredicate tx f =
  [ for t in tx do
      match t with
      | (_, Predicate p, _) -> yield f p ]

let mapObject tx f =
  [ for t in tx do
      match t with
      | (_, _, Object o) -> yield f o ]

let (<*-->) = mapSubject
let (<-*->) = mapPredicate
let (<--*>) = mapObject
(*Fold for statements about a subject*)
let foldS<'a> f (sx : Statements) (a : 'a) = asTriples sx |> List.fold f a
(*Map statements about a subject to 'a*)
let mapS<'a> f (t : Statements) (a : 'a) = asTriples t |> List.map f

module Constraints =
  let only = Seq.head
  let someof x = x
  let max n x = x

module Literal =
  open VDS.RDF
  let mapL f n =
    match n with
      | Node.Literal l -> Some (f l)
      | _ -> None

  let mapString = mapL (fun l -> l.Value)
  let mapInt = mapL (fun l -> int l.Value)
  let mapDateTime = mapL (fun l -> System.DateTime.Parse l.Value)
  let mapDateTimeOffset = mapL (fun l -> System.DateTimeOffset.Parse l.Value)
