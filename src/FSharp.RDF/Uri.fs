module Uri
open VDS.RDF
open VDS.RDF.Writing
open VDS.RDF.Parsing

let (++) a b = System.IO.Path.Combine(a, b)
type Uri = 
    | Uri of qname : string * segments : string list * ref : string option
    | Sys of System.Uri
    override x.ToString() = 
        match x with
        | Uri(q, p, Some r) -> 
            sprintf "%s:%s/%s" q (p |> List.reduce (++)) r
        | Uri(q, p, None) -> sprintf "%s:/%s" q (p |> List.reduce (++))
        | Sys uri -> string uri

let (=~) (x:Uri) (y:string) = (string x) = y

module ns = 
    let prov = "http://www.w3.org/ns/prov#"
    let rdf = "http://www.w3.org/1999/02/22-rdf-syntax-ns#"
    let owl = "http://www.w3.org/2002/07/owl#"
    let cnt = "http://www.w3.org/2011/content#"
    let compilation = "http://nice.org.uk/ns/compilation#"
    let git2prov = "http://nice.org.uk/ns/prov/"

    let add (g : IGraph, baseUri) = 
        g.BaseUri <- UriFactory.Create baseUri
        [("prov", prov)
         ("rdf", rdf)
         ("owl", owl)
         ("git2prov", git2prov)
         ("base", baseUri)
         ("compilation", compilation)
         ("cnt", cnt)]
        |> List.map (fun (p,ns) -> g.NamespaceMap.Add (p,UriFactory.Create ns))

let literal (g : IGraph) s = g.CreateLiteralNode s :> INode
let uri (g : IGraph) u = UriFactory.Create u |> g.CreateUriNode :> INode
let puri (g : IGraph) (u : Uri) = g.CreateUriNode(string u) :> INode
let qn (g : IGraph) (qn : string) = g.CreateUriNode qn :> INode
let a (g : IGraph) = qn g "rdf:type"
let date (g : IGraph) (d : System.DateTimeOffset) = 
    LiteralExtensions.ToLiteral(d, g) :> INode
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

