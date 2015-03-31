module Generator
open FSharp.RDF
open ProviderImplementation.ProvidedTypes
open OWL
open System.Reflection
open Microsoft.FSharp.Quotations
open VDS.RDF
open System.Linq
open System.Linq.Expressions
open Store
open OWLQueryable
open Gubbins

type GenerationContext =
    { ns : prefixes
      uri : Uri
      ont : string -> Class }

type OntologyNode(uri : string) =
    member x.Uri = Schema.Uri uri

type Class(uri) =
    inherit OntologyNode(uri)

type Individual(uri) =
    inherit OntologyNode(uri)

type ObjectProperty(uri) =
    inherit OntologyNode(uri)

type DataProperty(uri) =
    inherit OntologyNode(uri)

type Restriction(uri) =
    inherit OntologyNode(uri)

let ignoreBlank nx =
    nx |> Seq.filter (function
              | Node.Uri(Blank(b)) -> false
              | _ -> true)

let typeName (ns : prefixes) (uri : Uri) =
    let uri =
        match uri with
        | Schema.Uri(uri) -> uri

    let uri = System.Uri uri
    let matchingPrefix = List.tryFind (fun (p, u) -> uri.ToString().StartsWith(string u))
    match uri.Fragment, ns |> matchingPrefix with
    | NotEmpty fragment, Some("base", _) -> fragment.Substring(1)
    | NotEmpty fragment, Some(prefix, _) -> sprintf "%s:%s" prefix (fragment.Substring(1))
    | fragment, Some("base", _) -> uri.Segments.[uri.Segments.Length]
    | fragment, Some(prefix, _) -> sprintf "%s:%s" prefix (uri.Segments.[uri.Segments.Length])
    | _, _ -> string uri

let className (ns : prefixes) (cls : OWL.Class) =
    let uris =
        cls.EquivalentClasses
        |> Seq.map (typeName ns)
        |> List.ofSeq
    if uris.Length = 0 then typeName ns cls.Uri
    else sprintf "%s≡%s" (typeName ns cls.Uri) (uris |> String.concat "≡")

let restrictionDocs (ns : prefixes) (p : Schema.Property) =
    let (uri,_,cx) = p >>> "restriction %A"
    let rec restrictionName c = [
        match c with
        | uri,Constraint.Only rx -> yield sprintf "only (%s)" (rx
                                                |> Seq.map (typeName ns)
                                                |> Seq.reduce (sprintf "%s or %s"))
        | uri,Constraint.SomeOf rx -> yield sprintf "some (%s)" (rx
                                                |> Seq.map (typeName ns)
                                                |> Seq.reduce (sprintf "%s or %s"))
        | uri,Constraint.Further c -> yield! restrictionName (uri,c)
    ]
    let eachConstraint c = restrictionName (uri,c)
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

let objectProperty (ctx : GenerationContext) (p:OWL.ObjectProperty) =
  let (uri,ch,_) = p

  let restriction  =
    let restriction = ProvidedTypeDefinition(typeName ctx.ns uri,Some typeof<obj>)
    restriction.AddXmlDoc <| restrictionDocs ctx.ns p
    restriction

  let one p =
    let n = typeName ctx.ns ctx.uri
    let field = ProvidedField("_" + n, restriction)
    field.SetFieldAttributes(FieldAttributes.Private)
    let prop = ProvidedProperty(n, restriction, GetterCode = (fun [this] -> <@@ Expr.FieldGet(this, field) @@>))
    (prop, field, restriction)
  let some p =
    let lt = typedefof<List<_>>.MakeGenericType([| restriction :> System.Type |])
    restriction.AddMember lt
    let n = typeName ctx.ns ctx.uri
    let field = ProvidedField("_" + n, lt)
    field.SetFieldAttributes(FieldAttributes.Private)
    let prop = ProvidedProperty(n, lt, GetterCode = (fun [this] -> <@@ Expr.FieldGet(this, field) @@>))
    (prop, field, restriction)

  match ch with
    | Characteristics.Functional -> one p
    | _ -> some p

let individualType (ctx : GenerationContext) cs (rx : (ProvidedProperty * ProvidedField) list) =
    let t = ProvidedTypeDefinition("Individual", Some typeof<Individual>)

    let ctor =
        ProvidedConstructor
            (ProvidedParameter("self", typeof<Uri>)
             :: [ for (p, f) in rx -> ProvidedParameter(p.Name, typeof<Uri>) ])

    let ctorInfo =
        typeof<Individual>
            .GetConstructor(BindingFlags.Public ||| BindingFlags.Instance, null, [| typeof<string> |], null)
    ctor.BaseConstructorCall <- fun args -> ctorInfo, [ args.[0] ]
    ctor.InvokeCode <- fun (this :: args) ->
        rx
        |> List.mapi (fun i (p, f) -> <@@ Expr.FieldSet(this, f, Expr.Coerce((args.[i]), p.PropertyType)) @@>)
        |> List.reduce (fun a e -> Expr.Sequential(a, e))
    for (p, r) in rx do
        t.AddMember p
        t.AddMember r
    t.AddMember ctor
    (ctor, t)

let localisedAnnotations ax =
    [ for a in ax do
          match a with
          | Literal.Literal a -> yield a
          | Literal.LocalisedLiteral(c, a) -> yield a ]

let rec classNode (ctx : GenerationContext) =
    let cs = ctx.ont (string ctx.uri)
    let cls = ProvidedTypeDefinition(typeName ctx.ns ctx.uri, Some typeof<OntologyNode>)
    cls.AddXmlDoc(sprintf """
        <summary>
            Equivalents: %s
        </summary>
        <remarks>
            %s
        </remarks>
    """ (className ctx.ns cs) (cs.Comments
                               |> localisedAnnotations
                               |> Seq.fold (+) ""))
    let ctor = ProvidedConstructor([])
    let ctorInfo =
        typeof<Class>.GetConstructor(BindingFlags.Public ||| BindingFlags.Instance, null, [| typeof<string> |], null)
    ctor.BaseConstructorCall <- fun args -> ctorInfo, [ <@@ (ctx.uri) @@> ]
    ctor.InvokeCode <- fun args -> <@@ () @@>
    cls.AddMember ctor
    if cs.Subtypes.Any() then
        (fun () ->
        [ for sub in cs.Subtypes do
              yield classNode { ctx with uri = sub } ])
        |> cls.AddMembersDelayed
    if cs.ObjectProperties.Any() then
        let op = ProvidedTypeDefinition("ObjectProperties", Some typeof<obj>)
        cls.AddMember op
        (fun () ->
         [ for p in cs.ObjectProperties do
             let (uri,_,_) = p
             yield objectPropertyType { ctx with uri = uri } p ])
        |> op.AddMembersDelayed
    if cs.DataProperties.Any() then
        let op = ProvidedTypeDefinition("DataProperties", Some typeof<obj>)
        cls.AddMember op
        (fun () ->
        [ for (p, r) in cs.DataProperties do
              yield dataPropertyType { ctx with uri = p } r ])
        |> op.AddMembersDelayed
    let op = ProvidedTypeDefinition("Restrictions", Some typeof<obj>)
    cls.AddMember op
    (fun () ->
    [ let rx =
        [ for p in cs.ObjectProperties do
            let (uri,_,_) = p
            let (prop, field, restriction) = objectProperty { ctx with uri = uri } p
            op.AddMember restriction
            yield (prop, field) ]

      let (ctor, individualType) = individualType ctx cs rx
      let individuals =
          ProvidedMethod
              ("Individuals", [ ProvidedParameter("store", typeof<Store.store>) ],
               typedefof<IQueryable<_>>.MakeGenericType([| individualType :> System.Type |]),
               InvokeCode = (fun args -> <@@ () @@>), IsStaticMethod = true)
      for p, f in rx do
          yield p :> MemberInfo
          yield f :> MemberInfo
      yield individuals :> MemberInfo
      yield individualType :> MemberInfo ])
    |> cls.AddMembersDelayed
    cls

and objectPropertyType (ctx : GenerationContext) r = ProvidedTypeDefinition(typeName ctx.ns ctx.uri, Some typeof<obj>)

and dataPropertyType (ctx : GenerationContext) r = ProvidedTypeDefinition(typeName ctx.ns ctx.uri, Some typeof<obj>)

let root (t : ProvidedTypeDefinition) ns root ont =
    let cls =
        classNode { ns = ns
                    uri = root
                    ont = ont }
    t.AddMembers [ cls ]
    t.AddMember(ProvidedMethod("root", [], cls, InvokeCode = (fun a -> <@@ cls @@>)))
    t
