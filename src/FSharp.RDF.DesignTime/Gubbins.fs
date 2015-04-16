namespace global

module Gubbins = 
  open System.Text.RegularExpressions
  
  let (|Regex|_|) pattern input = 
    let m = Regex.Match(input, pattern)
    if m.Success then 
      Some(List.tail [ for g in m.Groups -> g.Value ])
    else None
  
  let (|NotEmpty|_|) str = 
    if not (System.String.IsNullOrEmpty str) then Some(str)
    else None

[<AutoOpen>]
module Logging = 
  let inline log l p x = 
    l p x
    x
  
  let inline logu l p x = 
    l p x
    ()
  
  let (>>>) p x = log printf x p
  let (>>|) p x = logu printf x
