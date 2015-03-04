module Uri

open VDS.RDF
open VDS.RDF.Writing
open VDS.RDF.Parsing

let (++) a b = System.IO.Path.Combine(a, b)

type Uri =
  | Curie of qname : string * segments : string list * ref : string option
  | Sys of System.Uri
  | VDS of IUriNode

  override x.ToString() =
    match x with
    | Curie(q, p, Some r) -> sprintf "%s:%s/%s" q (p |> List.reduce (++)) r
    | Curie(q, p, None) -> sprintf "%s:/%s" q (p |> List.reduce (++))
    | Sys uri -> string uri
    | VDS uri -> string uri.Uri

  static member (=) (u, u') =
    match u, u' with
    | Sys u, Sys u' -> u = u'
    | Sys u, VDS u' -> u = u'.Uri
    | VDS u, Sys u' -> u.Uri = u'
    | Curie _, Curie _ -> (string u) = (string u')
    | _ -> false

  static member from s = Uri.Sys (System.Uri(s))
  static member from s = Uri.Sys s

  static member toSys (x : Uri) = System.Uri(string x)

let (=~) (x : Uri) (y : string) = (string x) = y

type Node =
  | Uri of Uri
  | Blank of IBlankNode
  | Literal of ILiteralNode

  static member from (n : INode) =
    match n with
    | :? IUriNode as n -> Node.Uri(VDS n)
    | :? ILiteralNode as n -> Node.Literal n
    | :? IBlankNode as n -> Node.Blank n
    | _ -> failwith (sprintf "Unknown node %A" (n.GetType()))

  static member from c = Uri.Curie c |> Uri
  static member from (u:string) = Uri.from u |> Uri
  static member from (u:System.Uri) = Uri.from u |> Uri
  static member (=) (n, n') =
    match n, n' with
    | Uri n, Uri n' -> n = n'
    | _ -> false

type Subject =
  | Subject of Node
  static member from c = Subject(Node.Uri(Uri.Curie c))
  static member from (u:string) = Subject(Node.from u )

type Predicate =
  | Predicate of Node
  static member from c = Predicate(Node.Uri(Uri.Curie c))
  static member from u = Predicate(Node.Uri(Uri.Sys u))
  static member from u = Predicate(Node.Uri(Uri.Sys (System.Uri u)))

type Object =
  | Object of Node
  static member from c = Object(Node.Uri(Uri.Curie c))
  static member from u = Object(Node.Uri(Uri.Sys u))

type Triple = Subject * Predicate * Object

type Statements = Subject * (Predicate * Object) list

module ns =
  let prov = "http://www.w3.org/ns/prov#"
  let rdf = "http://www.w3.org/1999/02/22-rdf-syntax-ns#"
  let owl = "http://www.w3.org/2002/07/owl#"
  let cnt = "http://www.w3.org/2011/content#"
  let compilation = "http://nice.org.uk/ns/compilation#"
  let git2prov = "http://nice.org.uk/ns/prov/"

  let add (g : IGraph, baseUri) =
    g.BaseUri <- UriFactory.Create baseUri
    [ ("prov", prov)
      ("rdf", rdf)
      ("owl", owl)
      ("git2prov", git2prov)
      ("base", baseUri)
      ("compilation", compilation)
      ("cnt", cnt) ]
    |> List.iter
         (fun (p, ns) -> g.NamespaceMap.AddNamespace(p, UriFactory.Create ns))

module Assertion =
  let literal (g : IGraph) s = Literal(g.CreateLiteralNode s)
  let uri (g : IGraph) u = VDS(UriFactory.Create u |> g.CreateUriNode)
  let puri (g : IGraph) (u : Uri) = VDS(g.CreateUriNode(string u))
  let qn (g : IGraph) (qn : string) = VDS(g.CreateUriNode qn)
  let a (g : IGraph) = qn g "rdf:type"
  let date (g : IGraph) (d : System.DateTimeOffset) =
    (LiteralExtensions.ToLiteral(d, g))
  let triples (g : IGraph) = function
    | (s, px) ->
      px
      |> List.map (function
           | (p, o) -> Triple(s, p, o))
      |> g.Assert
      |> ignore
      ()

  let blank (g : IGraph) px =
    let b = g.CreateBlankNode()
    triples g (b, px) |> ignore
    b :> INode
