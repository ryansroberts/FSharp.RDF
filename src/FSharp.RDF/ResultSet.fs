namespace FSharp.RDF

module ResultSet = 
  open VDS.RDF
  open VDS.RDF.Query
  open Store
  
  let singles = function 
    | ResultSet rx -> rx |> Seq.map (fun r -> Node.from r.[0])
  let doubles = 
    function 
    | ResultSet rx -> 
      rx |> Seq.map (fun r -> (Node.from r.[0], Node.from r.[1]))
  let triples = 
    function 
    | ResultSet rx -> 
      rx 
      |> Seq.map (fun r -> (Node.from r.[0], Node.from r.[1], Node.from r.[2]))
