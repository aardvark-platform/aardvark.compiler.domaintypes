namespace DomainModel

open Aardvark.Base
open Aardvark.Base.Incremental

type Model =
    {
        fileName    : string
        bounds      : Box3d
    }

[<DomainType>]
type MyUnion =
    | A of int * hset<int>
    | B of string
    | C

[<DomainType>]
type Object =
    {
        [<PrimaryKey>]
        name        : string
        trafo       : Trafo3d
        model       : Model
    }

[<DomainType>]
type State =
    {
        [<TreatAsValue>]
        past        : Option<State>
        primary     : Option<Object>
        viewTrafo   : Trafo3d
        objects     : hset<Object>
        [<TreatAsValue>]
        test        : plist<Object>


        threads     : ThreadPool<int>
    }
