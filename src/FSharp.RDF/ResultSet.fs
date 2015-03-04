namespace FSharp.RDF
module ResultSet
open VDS.RDF
open VDS.RDF.Query

type ResultSet =
    | ResultSet of SparqlResultSet

let singles = function | ResultSet rx -> rx |> Seq.map (fun r -> r.[0])
let doubles = function | ResultSet rx -> rx |> Seq.map (fun r -> (r.[0],r.[1]))
let triples = function | ResultSet rx -> rx |> Seq.map (fun r -> (r.[0],r.[1],r.[2]))
