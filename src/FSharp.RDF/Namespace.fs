module Namespace

open VDS.RDF

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
