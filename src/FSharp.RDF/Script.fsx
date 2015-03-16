#r "../../packages/dotNetRDF/lib/net40/dotNetRDF.dll"
#r "../../packages/VDS.Common/lib/net40-client/VDS.Common.dll"
#r "../../packages/FSharpx.Core/lib/40/FSharpx.Core.dll"
#r "../../packages/Unquote/lib/net40/Unquote.dll"
#load "Graph.fs"
#load "Store.fs"

open Store
open Swensen.Unquote
open Graph

let compilerG = """
@prefix : <http://nice.org.uk/ns/compilation#> .
@prefix owl: <http://www.w3.org/2002/07/owl#> .
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix xml: <http://www.w3.org/XML/1998/namespace> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@base <http://nice.org.uk/ns/compilation/> .

:QualityStandards rdf:type :DirectoryPattern ,
                           owl:NamedIndividual ;
                  :expression "qualitystandards"^^xsd:string ;
                  :parent :Root .

:QualityStatement rdf:type :FilePattern ,
                           owl:NamedIndividual ;
                  :expression "statement-(?<QualityStatementId>\\s+)"^^xsd:string ;
                  :tool :Content ;
                  :parent :QualityStandard .

:QualityStandard  rdf:type :DirectoryPattern ,
                 owl:NamedIndividual ;
                 :expression "(?<QualityStandardId>\\s+)"^^xsd:string .
"""
let g = Graph.from compilerG
let root = Uri.from "http://nice.org.uk/ns/compilation#Root"
let directoryPattern = 
  Uri.from "http://nice.org.uk/ns/compilation#DirectoryPattern"
let expression = Uri.from "http://nice.org.uk/ns/compilation#expression"
let parent = Uri.from "http://nice.org.uk/ns/compilation#parent"

open resource

let roots = fromObject root
