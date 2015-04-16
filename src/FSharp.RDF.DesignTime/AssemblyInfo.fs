namespace System

open System.Reflection

[<assembly:AssemblyTitleAttribute("FSharp.RDF.DesignTime")>]
[<assembly:AssemblyProductAttribute("FSharp.RDF")>]
[<assembly:AssemblyDescriptionAttribute("FSharp interface to dotnetrdf")>]
[<assembly:AssemblyVersionAttribute("1.0")>]
[<assembly:AssemblyFileVersionAttribute("1.0")>]
do ()

module internal AssemblyVersionInformation = 
  [<Literal>]
  let Version = "1.0"
