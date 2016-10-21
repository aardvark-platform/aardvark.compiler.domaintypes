namespace Sepp

open Aardvark.Base
open Aardvark.Base.Incremental

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
        stuff : pset<int>
    }




