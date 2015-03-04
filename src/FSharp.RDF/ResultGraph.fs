module Traversal

open Uri
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
    use sr = new StreamReader(s)
    p.Load(g, sr)
    Graph g

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
        (Predicate(Node.from t.Predicate), Object(Node.from t.Subject)) ])

let asTriples x =
  match x with
  | (s, px) ->
    [ for (p, o) in px -> (s, p, o) ]

(*Triples for Predicate p*)
let pred p sx =
  match sx with
  | (s, px) ->
    px
    |> Seq.filter (function
         | Predicate p', _ -> p = Predicate p')
    |> Seq.map (fun (p, o) -> (s, p, o))

let traverse p (sx : Statements) : Statements seq =
  let traverse t =
    match t with
    | (_, _, Object(Node.Uri(Uri.VDS vds))) ->
      fromSubject (Uri.VDS vds) (Graph vds.Graph)  |> Option.Some
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
