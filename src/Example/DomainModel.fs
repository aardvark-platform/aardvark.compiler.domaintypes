namespace DomainModel

open Aardvark.Base
open Aardvark.Base.Incremental


type Model =
    {
        fileName    : string
        bounds      : Box3d
    }

[<DomainType>]
type Object =
    {
        trafo       : Trafo3d
        model       : Model
    }

[<DomainType>]
type State =
    {
        viewTrafo   : Trafo3d
        objects     : pset<Object>
    }




