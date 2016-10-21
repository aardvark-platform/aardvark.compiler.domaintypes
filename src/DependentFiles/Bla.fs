namespace Sepp

open Aardvark.Base

[<DomainType>]
type Object =
    {
        trafo : Trafo3d
        model : string
    }

[<DomainType>]
type State =
    {
        viewTrafo : Trafo3d
        objects : pset<Object>
    }




