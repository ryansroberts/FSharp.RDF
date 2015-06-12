namespace FSharp.RDF
[<AutoOpen>]
module JsonLD =
    open VDS.RDF
    open VDS.RDF.Writing
    open VDS.RDF.Writing.Formatting
    open JsonLD.Core
    open FSharp.RDF


    let namer () = UniqueNamer("_:b")

    let rec private triple (n:UniqueNamer) (d:RDFDataset) (S s,P p,O (o,xt)) =

      let obj =
        match o with
        | Uri x ->
          xt.Value
          |> Seq.collect Resource.asTriples
          |> Seq.iter (triple n d)

          Uri.toSys x |> string
        | Node.Blank (Blank xs) ->
          let name = n.GetName()
          let s = (S (name |> Uri.from))
          xs.Value
          |> Seq.map  (hasSubject s)
          |> Seq.iter (triple n d )

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

      match data with
        | Some data -> d.AddTriple (Uri.toSys s |> string,
                                    Uri.toSys p |> string,
                                    obj,
                                    data,
                                    "en")
        | None -> d.AddTriple (Uri.toSys s |> string,
                               Uri.toSys p |> string,
                               obj)

    type Resource with
      //Returns LD subgraph for the resource and all its asserted dependencies
      static member toJsonLD (o:JsonLdOptions) r =
        let n = namer()
        let d = RDFDataset()
        Resource.asTriples r
        |> Seq.iter (triple n d)

        JsonLdApi(o).FromRDF d
