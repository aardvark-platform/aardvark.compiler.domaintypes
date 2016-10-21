namespace Sepp
open Aardvark.Base

[<DomainType>]
type Object =
    {
        trafo : Trafo3d
    }

[<DomainType>]
type State =
    {
        a : int
        b : float
        objects : pset<Object>
    }
