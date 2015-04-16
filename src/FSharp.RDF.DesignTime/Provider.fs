module Providers

open ProviderImplementation.ProvidedTypes
open Microsoft.FSharp.Core.CompilerServices
open System.Reflection
open VDS.RDF
open OWL
open System
open System.IO
open Reasoning

[<TypeProvider>]
type Memory(config : TypeProviderConfig) as x = 
  inherit TypeProviderForNamespaces()
  do x.RegisterRuntimeAssemblyLocationAsProbingFolder config
  let ns = "LinkedData"
  let asm = Assembly.GetExecutingAssembly()
  let op = ProvidedTypeDefinition(asm, ns, "Memory", Some typeof<obj>)
  
  let parameters = 
    [ ProvidedStaticParameter("Path", typeof<string>)
      ProvidedStaticParameter("BaseUri", typeof<string>)
      ProvidedStaticParameter("NamespaceMappings", typeof<string>) ]
  
  let (++) x y = Path.Combine(x, y)
  
  let create() = 
    let init (typeName : string) (parameters : obj array) = 
      match parameters with
      | [| :? string as path; :? string as baseUri; :? string as nsmap |] -> 
        let erased = 
          ProvidedTypeDefinition(asm, ns, typeName, Some(typeof<obj>))
        let nsmap = parse nsmap
        let om = OntologyManager()
        let ctx = Reasoning.ReasoningContext.create (om.loadFile path)
        Generator.root erased nsmap (Uri.Uri baseUri) (om.schema ctx)
      | _ -> 
        raise (ArgumentException(sprintf "Invalid parameters %A" parameters))
    op.DefineStaticParameters(parameters, init)
    op
  
  do x.AddNamespace(ns, [ create() ])

[<TypeProviderAssembly>]
do ()
