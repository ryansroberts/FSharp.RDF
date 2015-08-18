namespace FSharp.RDF

open System
open ProviderImplementation.ProvidedTypes
open Microsoft.FSharp.Core.CompilerServices
open System.Reflection
open VDS.RDF
open System.IO
open FSharp.RDF.Ontology
open Microsoft.FSharp.Quotations

module private Generator =
  type GenerationContext =
    { uri : FSharp.RDF.Uri
      ont : string -> Class }

  type OntologyNode(uri : string) =
    member x.Uri = Uri.from uri

  type ClassNode(uri) =
    inherit OntologyNode(uri)

  type IndividualNode(uri) =
    inherit OntologyNode(uri)

  type ObjectPropertyNode(uri) =
    inherit OntologyNode(uri)

  type DataPropertyNode(uri) =
    inherit OntologyNode(uri)

  type RestrictionNode(uri) =
    inherit OntologyNode(uri)

  let typeName (Uri.Sys uri) = (string uri)

  let className (cls : Class) =
    let uris =
      cls.EquivalentClasses
      |> Seq.map typeName
      |> List.ofSeq
    if uris.Length = 0 then typeName cls.Uri
    else sprintf "%s≡%s" (typeName cls.Uri) (uris |> String.concat "≡")

  let restrictionDocs (uri, _, cx) =
    let rec restrictionName c =
      [ match c with
        | uri, Constraint.Only rx ->
          yield sprintf "only (%s)" (rx
                                     |> Seq.map typeName
                                     |> Seq.reduce (sprintf "%s or %s"))
        | uri, Constraint.SomeOf rx ->
          yield sprintf "some (%s)" (rx
                                     |> Seq.map typeName
                                     |> Seq.reduce (sprintf "%s or %s"))
        | uri, Constraint.Further c -> yield! restrictionName (uri, c) ]

    let eachConstraint c = restrictionName (uri, c)
    cx
    |> List.map eachConstraint
    |> List.concat
    |> List.reduce (fun a r -> a + "\r" + r)
    |> (sprintf """
           <summary>
                Property uri %s
                %s
           </summary>
       """ (string uri))

  let objectProperty (uri,c,cx) =

    let restriction = ProvidedTypeDefinition(typeName uri, Some typeof<ObjectPropertyNode>)
    restriction.AddXmlDocDelayed (fun ()-> restrictionDocs (uri,c,cx))
    restriction

  let individualType (ctx : GenerationContext) cs
      (rx : (ProvidedProperty * ProvidedField) list) =
    let t = ProvidedTypeDefinition("Individual", Some typeof<IndividualNode>)

    let ctor =
      ProvidedConstructor
        (ProvidedParameter("self", typeof<Uri>)
         :: [ for (p, f) in rx -> ProvidedParameter(p.Name, typeof<Uri>) ])

    let ctorInfo =
      typeof<IndividualNode>
        .GetConstructor(BindingFlags.Public ||| BindingFlags.Instance, null,
                        [| typeof<string> |], null)
    ctor.BaseConstructorCall <- fun args -> ctorInfo, [ args.[0] ]
    ctor.InvokeCode <- fun (this :: args) ->
      rx
      |> List.mapi
           (fun i (p, f) ->
           <@@ Expr.FieldSet(this, f, Expr.Coerce((args.[i]), p.PropertyType)) @@>)
      |> List.reduce (fun a e -> Expr.Sequential(a, e))
    for (p, r) in rx do
      t.AddMember p
      t.AddMember r
    t.AddMember ctor
    (ctor, t)

  let localisedAnnotations ax =
    [ for a in ax do
        match a with
        | Literal.String a -> yield a ]

  let rec classNode (ctx : GenerationContext) =
    let cs = ctx.ont (string ctx.uri)
    let cls =
      ProvidedTypeDefinition(typeName ctx.uri, Some typeof<OntologyNode>)
    cls.AddXmlDoc(sprintf """
            Equivalents: %s

            %s
            %A
    """ (className cs) (cs.Comments
                        |> localisedAnnotations
                        |> Seq.fold (+) "") cs) 
    let ctor = ProvidedConstructor([])
    let ctorInfo =
      typeof<Class>
        .GetConstructor(BindingFlags.Public ||| BindingFlags.Instance, null,
                        [| typeof<string> |], null)
    ctor.BaseConstructorCall <- fun args -> ctorInfo, [ <@@ (ctx.uri) @@> ]
    ctor.InvokeCode <- fun args -> <@@ () @@>
    cls.AddMember ctor
    if not (Set.isEmpty cs.Subtypes) then
      let op = ProvidedTypeDefinition("SubClasses", Some typeof<obj>)
      cls.AddMember op
      (fun () ->
       [ for sub in cs.Subtypes -> classNode { ctx with uri = sub } ])
      |> op.AddMembersDelayed
    if not (Set.isEmpty cs.ObjectProperties) then
      let op = ProvidedTypeDefinition("ObjectProperties", Some typeof<obj>)
      cls.AddMember op
      (fun () ->
       [ for p in cs.ObjectProperties -> objectProperty p ])
      |> op.AddMembersDelayed
    if not (Set.isEmpty cs.DataProperties) then
      let op = ProvidedTypeDefinition("DataProperties", Some typeof<obj>)
      cls.AddMember op
      (fun () ->
      [ for (p, r) in cs.DataProperties do
          yield dataPropertyType { ctx with uri = p } r ])
      |> op.AddMembersDelayed
      cls.AddMember op
    cls

  and objectPropertyType (ctx : GenerationContext) r =
    ProvidedTypeDefinition(typeName ctx.uri, Some typeof<obj>)

  and dataPropertyType (ctx : GenerationContext) r =
    ProvidedTypeDefinition(typeName ctx.uri, Some typeof<obj>)

  let root (t : ProvidedTypeDefinition) root ont =
    let cls =
      classNode { uri = root
                  ont = ont }
    t.AddMembers [ cls ]
    t.AddMember
      (ProvidedMethod("root", [], cls, InvokeCode = (fun a -> <@@ cls @@>)))
    t

module Provider =
    [<TypeProvider>]
    type Memory(config : TypeProviderConfig) as x =
      inherit TypeProviderForNamespaces()
      do x.RegisterRuntimeAssemblyLocationAsProbingFolder config
      let ns = "FSharp.RDF"
      let asm = Assembly.GetExecutingAssembly()
      let op = ProvidedTypeDefinition(asm, ns, "OntologyProvider", Some typeof<obj>)

      let parameters =
        [ ProvidedStaticParameter("Path", typeof<string>)
          ProvidedStaticParameter("BaseUri", typeof<string>) ]

      let (++) x y = Path.Combine(x, y)

      let create() =
        let init (typeName : string) (parameters : obj array) =
          match parameters with
          | [| :? string as path; :? string as baseUri; |] ->
            let erased =
              ProvidedTypeDefinition(asm, ns, typeName, Some(typeof<obj>))
            let ctx = Ontology.loadFile path
            Generator.root erased (Uri.from baseUri) (Ontology.cls ctx)
          | e ->
            raise
              (ArgumentException(sprintf "Invalid parameters %A %A" parameters e))
        op.DefineStaticParameters(parameters, init)
        op

      do x.AddNamespace(ns, [ create() ])

[<TypeProviderAssembly>]
do ()
