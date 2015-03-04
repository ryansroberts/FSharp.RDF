module ResultSet

open VDS.RDF
open VDS.RDF.Query
open Uri
open Store

let singles = function 
  | ResultSet rx -> rx |> Seq.map (fun r -> Node.fromVDS r.[0])
let doubles = 
  function 
  | ResultSet rx -> 
    rx |> Seq.map (fun r -> (Node.fromVDS r.[0], Node.fromVDS r.[1]))
let triples = 
  function 
  | ResultSet rx -> 
    rx 
    |> Seq.map 
         (fun r -> (Node.fromVDS r.[0], Node.fromVDS r.[1], Node.fromVDS r.[2]))
