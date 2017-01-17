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
        name        : string
        trafo       : Trafo3d
        model       : Model
    }

[<DomainType>]
type State =
    {
        primary     : Object
        viewTrafo   : Trafo3d
        objects     : pset<Object>
        test        : array<Object>
    }


//module Bla =
//    let initial = 
//        { 
//            primary = { name = "hugo"; trafo = Trafo3d.Identity; model = { fileName = "asdasd"; bounds = Box3d.Invalid }}
//            viewTrafo = Trafo3d.Identity
//            objects = PSet.empty
//            test = [||]
//        }
