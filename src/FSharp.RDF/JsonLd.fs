namespace FSharp.RDF
[<AutoOpen>]
module JsonLD =
    open VDS.RDF
    open VDS.RDF.Writing
    open VDS.RDF.Writing.Formatting
    open JsonLD.Core
    open FSharp.RDF


    let namer () = UniqueNamer("_:b")

    //Handle jsonld blank nodes seperately from underlying subject
    type LS =
      | S' of Subject
      | BNode of string

    let private liftBS (s,p,o) = (S' s,p,o)

    let rec private triple (n:UniqueNamer) (d:RDFDataset) (s:LS,P p,O (o,xt)) =

      let obj =
        match o with
        | Uri x ->
          xt.Value
          |> Seq.collect Resource.asTriples
          |> Seq.map liftBS
          |> Seq.iter (triple n d)
          Uri.toSys x |> string

        | Node.Blank (Blank xs) ->
          let name = n.GetName()
          xs.Value
          |> Seq.map  (fun (p,o) -> (BNode name,p,o))
          |> Seq.iter (triple n d)

          name
        | Literal x ->
          match x with
            | String x -> x
            | DateTimeOffset x -> string x

      let data =
        match o with
          | Literal x ->
            match x with
              | String x -> Some "xsd:string"
              | DateTimeOffset x -> Some "xsd:datetimeoffset"
          | _ -> None

      let subject =
        match s with
          | S' (Subject.S s) -> Uri.toSys s |> string
          | BNode s -> s

      match data with
        | Some data -> d.AddTriple (subject,
                                    Uri.toSys p |> string,
                                    obj,
                                    data,
                                    "en")
        | None -> d.AddTriple (subject,
                               Uri.toSys p |> string,
                               obj)

    type Resource with
      //Returns LD subgraph for the resource and all its asserted dependencies
      static member toJsonLD (o:JsonLdOptions) r =
        let n = namer()
        let d = RDFDataset()
        Resource.asTriples r
        |> Seq.map liftBS
        |> Seq.iter (triple n d)

        let api = JsonLdApi(o)
        api.Normalize(d) |> ignore
        api.FromRDF d
