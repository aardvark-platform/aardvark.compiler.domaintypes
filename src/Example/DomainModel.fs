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
        [<PrimaryKey>]
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

type MObject private(__initial : DomainModel.Object) =
    let mutable __current = __initial
    let _name = ResetMod(__initial.name)
    let _trafo = ResetMod(__initial.trafo)
    let _model = ResetMod(__initial.model)

    member x.name = _name :> IMod<_>
    member x.trafo = _trafo :> IMod<_>
    member x.model = _model :> IMod<_>

    member x.Update(__model : DomainModel.Object) =
        if not (Object.ReferenceEquals(__model, __current)) then
            __current <- __model
            _name.Update(__model.name)
            _trafo.Update(__model.trafo)
            _model.Update(__model.model)
    static member Create(initial) = MObject(initial)

namespace DomainModel
open System
open Aardvark.Base.Incremental

type MState private(__initial : DomainModel.State) =
    let mutable __current = __initial
    let _primary = MObject.Create(__initial.primary)
    let _viewTrafo = ResetMod(__initial.viewTrafo)
    let _objects = ResetMapSet((fun v -> v.name :> obj), __initial.objects, MObject.Create, fun (m,i) -> m.Update(i))
    let _test = ResetMod(__initial.test)

    member x.primary = _primary
    member x.viewTrafo = _viewTrafo :> IMod<_>
    member x.objects = _objects :> aset<_>
    member x.test = _test :> IMod<_>

    member x.Update(__model : DomainModel.State) =
        if not (Object.ReferenceEquals(__model, __current)) then
            __current <- __model
            _primary.Update(__model.primary)
            _viewTrafo.Update(__model.viewTrafo)
            _objects.Update(__model.objects)
            _test.Update(__model.test)
    static member Create(initial) = MState(initial)