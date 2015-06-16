module FSharp.RDF.Ontology
open FSharp.RDF
open org.semanticweb.owlapi.apibinding
open org.semanticweb.owlapi.model
open org.semanticweb.owlapi.util
open org.semanticweb.owlapi.io
open org.coode.owlapi.manchesterowlsyntax
open org.semanticweb.owlapi.vocab
open System.Collections.Generic
open org.semanticweb.owlapi.reasoner
open org.semanticweb.owlapi.reasoner.structural
open uk.ac.manchester.cs.owlapi.modularity
open uk.ac.manchester.cs.owl.owlapi
open org.semanticweb.owlapi.apibinding
open org.semanticweb.owlapi.model
open org.semanticweb.owlapi.reasoner
open Microsoft.FSharp.Linq.RuntimeHelpers

type Constraint =
  | SomeOf of Set<Uri>
  | Only of Set<Uri>
  | Value of Uri
  | Minimum of int * Uri
  | Maximum of int * Uri
  | Exactly of int * Uri
  | Further of Constraint

[<System.Flags>]
type Characteristics =
  | None              = 0b000000000
  | Functional        = 0b000000001
  | InverseFunctional = 0b000000010
  | Transitive        = 0b000000100
  | Symmetric         = 0b000001000
  | Asymmetric        = 0b000010000
  | Reflexive         = 0b000100000
  | Irreflexive       = 0b001000000

type Property = Uri * Characteristics * Constraint list

type ObjectProperty = Property

type DataProperty = Property

type Class =
  { Uri : Uri
    Label : Literal list
    Comments : Literal list
    ObjectProperties : Set<ObjectProperty>
    DataProperties : Set<Uri * Set<Uri>>
    Subtypes : Set<Uri>
    Supertypes : Set<Uri>
    EquivalentClasses : Set<Uri> }

type Ontology =
  | Ontology of OWLOntology

type Reasoner =
  | Reasoner of OWLReasoner

type DataFactory =
  | Factory of OWLDataFactory

type Uri with
  static member fromIRI (iri : IRI) = Uri.from (iri.toString())
  static member fromHasUri (has : HasIRI) = Uri.from ((has.getIRI()).toString())

let rec iter<'a> (nx : java.util.Set) =
  match nx with
  | :? NodeSet as nx -> iter<'a> (nx.getFlattened())
  | _ ->
    [ let i = nx.iterator()
      while i.hasNext() do
        yield i.next() :?> 'a ]

let rec splitIntersections (c : obj) =
  [ match c with
    | :? OWLObjectIntersectionOf as x ->
      yield! x.getClassesInSignature()
             |> iter<OWLEntity>
             |> List.map splitIntersections
             |> List.concat
    | :? HasIRI as x -> yield Uri.fromHasUri x ]

type ReasoningContext =
  { Ontology : OWLOntology
    Reasoner : OWLReasoner
    DataFactory : OWLDataFactory }
  static member create (o, r, f) =
    match o, r, f with
    | Ontology o, Reasoner r, Factory f ->
      { Ontology = o
        Reasoner = r
        DataFactory = f }

let characteristicsOf ctx (p : OWLPropertyExpression) =
  let test f p =
    if f then p
    else Characteristics.None

  let isFunctionalObject p =
    ctx.DataFactory.getOWLObjectMinCardinality (2, p)
    |> ctx.Reasoner.isSatisfiable
    |> not

  let isFunctionalData p =
    ctx.DataFactory.getOWLDataMinCardinality (2, p)
    |> ctx.Reasoner.isSatisfiable
    |> not

  match p with
  | :? OWLObjectProperty as p ->
    test (isFunctionalObject (p)) Characteristics.Functional
    ||| test (p.isInverseFunctional (ctx.Ontology))
          Characteristics.InverseFunctional
    ||| test (p.isTransitive (ctx.Ontology)) Characteristics.Transitive
    ||| test (p.isSymmetric (ctx.Ontology)) Characteristics.Symmetric
    ||| test (p.isAsymmetric (ctx.Ontology)) Characteristics.Asymmetric
    ||| test (p.isReflexive (ctx.Ontology)) Characteristics.Reflexive
    ||| test (p.isIrreflexive (ctx.Ontology)) Characteristics.Irreflexive
  | :? OWLDataProperty as p ->
    test (isFunctionalData (p)) Characteristics.Functional

(*Vistors look like hell in fs*)
type propertyExtractor(ctx) =
  let mutable extracted : Set<Uri * Characteristics * Constraint> = Set.empty
  member x.Extracted() = extracted

  member x.extract (node : OWLQuantifiedRestriction) f =
    let prop = node.getProperty()
    if (prop.isAnonymous()) then x.visitNode (prop)
    else
      let uriOf p = Uri.from ((string p).Substring(1, (string p).Length - 2))
      (*Hack, find better way to extract uri*)
      extracted <- extracted.Add(uriOf prop, characteristicsOf ctx prop,
                                 f (node.getFiller().getClassesInSignature()
                                    |> iter<OWLClass>
                                    |> List.map Uri.fromHasUri
                                    |> Set.ofList))

  member private x.visitNode (node : OWLObject) : unit =
    match node with
    | :? OWLObjectIntersectionOf as e -> e.accept x
    | :? OWLObjectComplementOf as e -> e.accept x
    | :? OWLObjectUnionOf as e -> e.accept x
    | :? OWLObjectAllValuesFrom as e -> e.accept x
    | :? OWLObjectSomeValuesFrom as e -> e.accept x
    | :? OWLObjectHasValue as e -> e.accept x
    | :? OWLObjectMinCardinality as e -> e.accept x
    | :? OWLObjectExactCardinality as e -> e.accept x
    | :? OWLObjectMaxCardinality as e -> e.accept x
    | :? OWLObjectHasSelf as e -> e.accept x
    | :? OWLObjectOneOf as e -> e.accept x
    | :? OWLDataMinCardinality as e -> e.accept x
    | :? OWLDataExactCardinality as e -> e.accept x
    | :? OWLDataMaxCardinality as e -> e.accept x
    | sc -> ()
    ()

  interface OWLClassExpressionVisitor with

    member x.visit (node : OWLClass) =
      node.getSuperClasses (ctx.Ontology)
      |> iter<OWLObject>
      |> List.iter (x.visitNode)
      ()

    member x.visit (node : OWLObjectIntersectionOf) =
      node.getOperands()
      |> iter<OWLClass>
      |> List.iter (fun e -> e.accept (x))
      ()

    member x.visit (node : OWLObjectComplementOf) = node.getOperand().accept(x)

    member x.visit (node : OWLObjectUnionOf) =
      node.getOperands()
      |> iter<OWLClass>
      |> List.iter (fun e -> e.accept (x))
      ()

    member x.visit (node : OWLDataAllValuesFrom) = x.extract node SomeOf
    member x.visit (node : OWLDataSomeValuesFrom) = x.extract node SomeOf
    member x.visit (node : OWLDataHasValue) = ()
    member x.visit (node : OWLObjectAllValuesFrom) = x.extract node Only
    member x.visit (node : OWLObjectSomeValuesFrom) = x.extract node SomeOf
    member x.visit (node : OWLObjectHasValue) = ()
    member x.visit (node : OWLObjectMinCardinality) = x.extract node SomeOf
    member x.visit (node : OWLObjectExactCardinality) = x.extract node SomeOf
    member x.visit (node : OWLObjectMaxCardinality) = x.extract node SomeOf
    member x.visit (node : OWLObjectHasSelf) = ()
    member x.visit (node : OWLObjectOneOf) = ()
    member x.visit (node : OWLDataMinCardinality) = x.extract node SomeOf
    member x.visit (node : OWLDataExactCardinality) = x.extract node SomeOf
    member x.visit (node : OWLDataMaxCardinality) = x.extract node SomeOf

let extractIri ex = ex |> List.map Uri.fromHasUri

let objectProperties ctx (c : OWLClass) =
  let firstCharacteristic cx =
    cx
    |> Seq.map (fun (_, x, _) -> x)
    |> Seq.head

  let constraints cx =
    cx
    |> Seq.map (fun (_, _, x) -> x)
    |> List.ofSeq

  let px = propertyExtractor ctx
  (px :> OWLClassExpressionVisitor).visit(c)
  px.Extracted()
  |> Seq.groupBy (fun (k, _, _) -> k)
  |> Seq.map (fun (k, cx) -> (k, firstCharacteristic cx, constraints cx))
  |> Set.ofSeq

let subTypes ctx (c : OWLClass) =
  ctx.Reasoner.getSubClasses(c, true).getFlattened()
  |> iter<OWLClass>
  |> List.map Uri.fromHasUri

let superTypes ctx (c : OWLClass) =
  ctx.Reasoner.getSuperClasses(c, true).getFlattened()
  |> iter<OWLClass>
  |> List.map Uri.fromHasUri

let labels ctx (e : OWLEntity) =
  e.getAnnotations (ctx.Ontology)
  |> iter<OWLAnnotation>
  |> List.filter (fun a -> a.getProperty().isLabel())
  |> List.map (fun a -> Literal.String(string (a.getValue())))

let comments ctx (e : OWLEntity) =
  e.getAnnotations (ctx.Ontology)
  |> iter<OWLAnnotation>
  |> List.filter (fun a -> a.getProperty().isComment())
  |> List.map (fun a -> Literal.String(string (a.getValue())))

