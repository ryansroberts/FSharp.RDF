module Walk

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

module private GetTriples =
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

  let triplesToStatement (t : Triple seq) =
    t
    |> Seq.groupBy (fun t -> t.Subject)
    |> Seq.map
         (fun (s, tx) ->
         R ((Subject(Node.from s)),
         [ for t in tx ->
             (Predicate(Node.from t.Predicate), Object(Node.from t.Object)) ]))

  let fromSingle (f : Uri -> IGraph -> Triple seq) x g =
    match g with
    | Graph g -> f x g |> triplesToStatement

  let fromDouble (f : Uri -> Uri -> IGraph -> Triple seq) x y g =
    match g with
    | Graph g -> f x y g |> triplesToStatement

open GetTriples

let fromSubject<'a> u g = fromSingle bySubject u g
let fromPredicate<'a> u g = fromSingle byPredicate u g
let fromObject<'a> u g = fromSingle byObject u g
let fromPredicateObject<'a> x y g = fromDouble byPredicateObject x y g
let fromSubjectObject<'a> x y g = fromDouble bySubjectObject x y g
let fromSubjectPredicate<'a> x y g = fromDouble bySubjectPredicate x y g

let asTriples x =
  match x with
  | (s, px) ->
    [ for (p, o) in px -> (s, p, o) ]

type Walker<'a> =
  | W of (Resource seq -> ('a * Resource seq) seq)

module walker =
  //Resource matching condition c
  let pred c (R (s,px)) = R (s, px |> List.filter c)

  //Resource for predicate p
  let forPredicate p = pred (fun (p',_) -> p = p')

  //Applies f to the subject component of statements
  let mapSubject f (R (s,_)) = [f s]

  //Applies f to the predicate component of statements
  let mapPredicate f (R (s,px)) =
    seq {for p in px do
         match p with
         | (Predicate p, _) -> yield f p }

  //Applies f to the predicate component of all resources
  let mapPredicates f = Seq.map ( mapPredicate f)

  //Applies f to the object component of statements
  let mapObject f (R (s,px)) =
    seq {for p in px do
         match p with
         | (_, Object o) -> yield f o }

  //Applies f to the object component of all resources
  let mapObjects f = Seq.collect (mapObject f)

  //Traverses object properties for predicate pr
  let mapNext pr sx =
    let (R (s,px)) = (forPredicate pr) sx
    seq {for p in px do
            match p with
            | (_, Object(Node.Uri(Uri.VDS vds))) ->
                yield! bySubject (Uri.VDS vds) (vds.Graph) |> triplesToStatement
            | _ -> () }

  let mapAllNext pr = Seq.map (mapNext pr) >> Seq.concat

  let fromSeq sx = W(fun sx' -> seq {yield ((), sx) })

  let unit x = W(fun sx ->  seq {yield (x, sx)})
  //Run first , then second generated by f
  let bind f (W w) =
    W(fun sx ->
      seq{ for (v, sx) in w sx do
           printfn "Bind val - %A" v
           let (W w') = f v
           yield! w' sx })
  //Parser that does nothing
  let zero() = W(fun sx -> Seq.empty)
  //Results of first walker, followed by results of second
  let combine (W w) (W w') =
    W(fun sx ->
      let sx' = w sx |> Seq.collect snd
      Seq.concat [(w sx);(w' sx')]
      )

  //Produce a walker where producing f applied to object component of statements
  let mapO pr f (W w) =
    W(fun sx ->
      seq {for (v, s) in w sx do
           let s' = Seq.map (forPredicate pr) s
           yield (mapObjects f s',sx)})

  let traverse pr (W w) = W(fun sx -> w (mapAllNext pr sx))

  let run (W w) =
    w
    >> Seq.map fst

module xsd =
  open VDS.RDF

  let mapL f n =
        match n with
        | Node.Literal l -> Some (f l)
        | _ -> None

  let string = mapL (fun l -> l.Value)
  let int = mapL (fun l -> int l.Value)
  let datetime = mapL (fun l -> System.DateTime.Parse l.Value)
  let datetimeoffset = mapL (fun l -> System.DateTimeOffset.Parse l.Value)

type WalkBuilder() =
  member this.Zero = walker.zero
  member this.Bind(t, f) = walker.bind f t
  member this.Yield v = walker.unit v
  member this.YieldFrom(W w) = (W w)
  member this.Combine(x, y) = walker.combine x y
  member this.For(t, f) = walker.bind f t
  //Lifted for that takes a statement seq
  member this.For(sx, f) = W(fun _ -> seq{yield (f sx, sx)})

  member x.Delay(f) =
    W(fun sx ->
      let (W w) = f()
      w sx)

  //Apply f to object for statements
  [<CustomOperation("mapO", AllowIntoPattern = true, MaintainsVariableSpace = true)>]
  member x.mapO (p, f, pr) = walker.mapO f pr p

  [<CustomOperation("traverse", AllowIntoPattern = true,
                    MaintainsVariableSpace = true)>]
  member x.traverse (t, pr) = walker.traverse pr t

let walk = new WalkBuilder()

module Combinators =
  let rec oneOrMore p = walk { let! x = p
                               let! xs = zeroOrMore p
                               yield x :: xs }

  and zeroOrMore p =
    walk {
      yield! oneOrMore p
      yield []
      }

  let (<+>) p q = walker.combine p q
