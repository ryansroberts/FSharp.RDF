namespace FSharp.RDF
[<AutoOpen>]
module JsonLD =
    open VDS.RDF
    open VDS.RDF.Writing
    open VDS.RDF.Writing.Formatting
    open JsonLD.Core
    open FSharp.RDF

    type Config =
      | Config of JsonLdOptions

    [<AutoOpen>]
    module config =
     type Config with
       static member context x (Config c) = c.SetExpandContext x;(Config c)
       static member compactArrays x (Config c) = c.SetCompactArrays x;(Config c)
       static member useRDFType x (Config c) = c.SetUseRdfType x;(Config c)
       static member useNativeTypes x (Config c) = c.SetUseNativeTypes x;(Config c)
       static member produceGeneralisedRdf x (Config c) = c.SetProduceGeneralizedRdf x;(Config c)
       static member omitDefault x (Config c) = c.SetOmitDefault x;(Config c)
       static member setExplicit x (Config c) = c.SetExplicit x;(Config c)
       static member setEmbed x (Config c) = c.SetEmbed x;(Config c)

    let namer () = UniqueNamer("_:b")

    //Lifted type to jsonld blank nodes seperately from underlying subject
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
      static member toJsonLD (o:JsonLdOptions) xr =
        let n = namer()
        let d = RDFDataset()
        Seq.collect Resource.asTriples xr
        |> Seq.map liftBS
        |> Seq.iter (triple n d)

        let api = JsonLdApi(o)
        api.Normalize(d) |> ignore
        api.FromRDF d
