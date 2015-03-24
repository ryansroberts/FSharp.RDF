namespace FSharp.RDF
module Schema =
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

    type Property = Uri * Characteristics * (Constraint list)
