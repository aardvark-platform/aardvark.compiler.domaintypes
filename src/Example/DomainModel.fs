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
        objects     : hset<Object>
        test        : array<Object>
    }


namespace DomainModel
open System
open Aardvark.Base.Incremental


type MObject(__initial : DomainModel.Object) =
    let mutable __current = __initial
    let _name = ResetMod(__initial.name)
    let _trafo = ResetMod(__initial.trafo)
    let _model = ResetMod(__initial.model)

    member x.name = _name
    member x.trafo = _trafo
    member x.model = _model

    member x.Apply(__model : DomainModel.Object) =
        if not (Object.ReferenceEquals(__model, __current)) then
            __current <- __model
            _name.Update(__model.name)
            _trafo.Update(__model.trafo)
            _model.Update(__model.model)

namespace DomainModel
open System
open Aardvark.Base.Incremental


type MState(__initial : DomainModel.State) =
    let mutable __current = __initial
    let _primary = MObject(__initial.primary)
    let _viewTrafo = ResetMod(__initial.viewTrafo)
    let _objects = ResetMod(__initial.objects)
    let _test = ResetMod(__initial.test)

    member x.primary : MObject = _primary
    member x.viewTrafo = _viewTrafo
    member x.objects = _objects
    member x.test = _test

    member x.Apply(__model : DomainModel.State) =
        if not (Object.ReferenceEquals(__model, __current)) then
            __current <- __model
            _primary.Apply(__model.primary)
            _viewTrafo.Update(__model.viewTrafo)
            _objects.Update(__model.objects)
            _test.Update(__model.test)

//module Bla =
//    let initial = 
//        { 
//            primary = { name = "hugo"; trafo = Trafo3d.Identity; model = { fileName = "asdasd"; bounds = Box3d.Invalid }}
//            viewTrafo = Trafo3d.Identity
//            objects = PSet.empty
//            test = [||]
//        }
