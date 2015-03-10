module Graph

open VDS.RDF
open VDS.RDF.Writing
open VDS.RDF.Parsing

let (++) a b = System.IO.Path.Combine(a, b)

[<CustomEquality; NoComparison>]
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

  override u.Equals(u') =
    match u' with
    | :? Uri as u' ->
      match u, u' with
      | Sys u, Sys u' -> (string u) = (string u')
      | VDS u, VDS u' -> u = u'
      | Sys u, VDS u' -> (string u) = (string u'.Uri)
      | VDS u, Sys u' -> (string u.Uri) = (string u')
      | Curie _, Curie _ -> (string u) = (string u')
      | _ -> false
    | _ -> false

  static member from s = Uri.Sys(System.Uri(s))
  static member from s = Uri.Sys s
  static member toSys (x : Uri) = System.Uri(string x)

[<CustomEquality; NoComparison>]
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

  static member from c = Node.Uri(Uri.Curie c)
  static member from (u : string) = Node.Uri(Uri.from u)
  static member from (u : System.Uri) = Node.Uri(Uri.from u)
  override n.Equals(n') =
    match n' with
    | :? Node as n' ->
      match n, n' with
      | Uri n, Uri n' -> n = n'
      | _ -> false
    | _ -> false

type Subject =
  | Subject of Node
  static member from c = Subject(Node.Uri(Uri.Curie c))
  static member from (u : string) = Subject(Node.from u)

type Predicate =
  | Predicate of Node
  static member from c = Predicate(Node.Uri(Uri.Curie c))
  static member from u = Predicate(Node.Uri(Uri.Sys u))
  static member from u = Predicate(Node.Uri(Uri.Sys(System.Uri u)))

type Object =
  | Object of Node
  static member from c = Object(Node.Uri(Uri.Curie c))
  static member from u = Object(Node.Uri(Uri.Sys u))

type Triple = Subject * Predicate * Object

type Statements =
  | S of Subject * (Predicate * Object) list
