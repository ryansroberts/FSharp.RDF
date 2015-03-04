module Graph
open VDS.RDF
open Store

type Literal =
    | Literal of string

type Node =
    | Uri of IUriNode
    | Blank of IBlankNode
    | Literal of ILiteralNode
with static member fromVDS (n:INode) =
    match n with
        | :? IUriNode as n -> Node.Uri n
        | :? ILiteralNode as n -> Node.Literal n
        | :? IBlankNode as n -> Node.Blank n
        | _ -> failwith (sprintf "Unknown node %A" (n.GetType ()))

type Subject =
    | Subject of Node

type Predicate =
    | Predicate of Node

type Object =
    | Object of Node

type Triple =
    | Triple of (Subject * Predicate * Object)
    with static member fromVDS (t:VDS.RDF.Triple) =
        Triple (Subject ( Node.fromVDS t.Subject ),Predicate (Node.fromVDS t.Predicate),Object (Node.fromVDS t.Object))


let graphTriples s =
    match s with
        | Store.Memory g ->
        g.Triples
        |> Seq.map Triple.fromVDS

let tripleMatchingSubject s sub =
    match s with
        | Store.Memory g ->
        match sub with
            | Subject (Node.Uri u) -> g.GetTriplesWithSubject u

let tripleWithPredicateObject s pre ob =
    match s with
        | Store.Memory g ->
        match pre,ob with
            | Node.Uri pre,Node.Uri ob -> g.GetTriplesWithPredicateObject (pre,ob)


let rec walk<'a> (f:Triple -> 'a -> 'a) (t:Triple) (a:'a) =
    let a' = f t a
    let traverse tx =
        tx |> Seq.map Triple.fromVDS
        |> Seq.fold (fun a'' t -> walk f t a'') a'
    match t with
        | Triple (_,_,Object ( Node.Uri o )) ->
            o.Graph.GetTriplesWithSubject o
            |> traverse
        | Triple (_,_,Object ( Node.Blank o )) ->
            o.Graph.GetTriplesWithSubject o
            |> traverse
        | _ -> a'

let (|UriIs|_|) n s =
    match n with
        | Node.Uri n' when (string n') = s -> Some n
        | _ -> None

let (|RdfType|_|) n =
    match n with
        | UriIs n' when ( ns.rdf + "type" ) -> Some n
        | _ -> None
