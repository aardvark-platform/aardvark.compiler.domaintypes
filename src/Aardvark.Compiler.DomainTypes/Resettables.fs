namespace Aardvark.Base.Incremental

open System
open System.Threading
open System.Runtime.CompilerServices
open System.Collections.Generic
open Aardvark.Base

type IUpdatable<'a> =
    abstract member Update : 'a -> unit

type ResetMod<'a>(value : 'a) =
    inherit AdaptiveObject()

    let mutable value = value
    let mutable cache = value
    
    member x.UpdateNoEquality(v : 'a) =
        if not <| Object.ReferenceEquals(v, value) then
            value <- v
            x.MarkOutdated()

    member x.Update(v : 'a) =
        if not <| Object.Equals(v, value) then
            value <- v
            x.MarkOutdated()
        

    member x.GetValue(token : AdaptiveToken) =
        x.EvaluateAlways token (fun token ->
            if x.OutOfDate then
                cache <- value
            
            cache
        )

    override x.ToString() =
       sprintf "{ value = %A }" value

    interface IUpdatable<'a> with
        member x.Update v = x.Update v

    interface IMod with
        member x.IsConstant = false
        member x.GetValue(caller) = x.GetValue(caller) :> obj

    interface IMod<'a> with
        member x.GetValue(caller) = x.GetValue(caller)

type ResetSet<'a>(initial : hset<'a>) =
    let history = History HRefSet.traceNoRefCount
    do initial |> Seq.map Add |> HDeltaSet.ofSeq |> history.Perform |> ignore

    let mutable current = initial

    member x.Update(values : hset<'a>) =
        let ops = HSet.computeDelta current values
        current <- values
        history.Perform ops |> ignore

    override x.ToString() =
        current.ToString()
        
    interface IUpdatable<hset<'a>> with
        member x.Update v = x.Update v

    interface aset<'a> with
        member x.IsConstant = false
        member x.GetReader() = history.NewReader()
        member x.Content = history :> IMod<_>

type ResetMapSet<'k, 'v>(getId : 'k -> obj, initial : hset<'k>, create : 'k -> 'v, update : 'v * 'k -> unit) =
    let history = History HRefSet.traceNoRefCount
    let cache = Dict<obj, ref<'k> * 'v>()

    let mutable current = HSet.empty

    let update (keys : hset<'k>) =
        let keyDeltas = HSet.computeDelta current keys

        
        let valueDeltas =
            keyDeltas |> HDeltaSet.choose (fun d ->
                match d with
                    | Add(_,k) ->
                        let mutable isNew = false
                        let r, v = 
                            cache.GetOrCreate(getId k, fun _ ->
                                isNew <- true
                                ref k, create k
                            )

                        if isNew then
                            Some (Add v)
                        else
                            r := k
                            None

                    | Rem(_,k) ->
                        match cache.TryRemove k with
                            | (true, (_,v)) ->
                                Some (Rem v)
                            | _ ->
                                None
            )



        current <- keys
        history.Perform valueDeltas |> ignore
        for (r, v) in cache.Values do
            update(v, !r)
        
    do update initial

    member x.Update(keys : hset<'k>) =
        update keys

    override x.ToString() =
        current.ToString()
        
    interface IUpdatable<hset<'k>> with
        member x.Update v = x.Update v

    interface aset<'v> with
        member x.IsConstant = false
        member x.GetReader() = history.NewReader()
        member x.Content = history :> IMod<_>

    new(initial : hset<'k>, create : 'k -> 'v, update : 'v * 'k -> unit) = ResetMapSet(unbox, initial, create, update)

type ResetList<'a>(initial : plist<'a>) =
    let history = History PList.trace
    do 
        let delta = plist.ComputeDeltas(PList.empty, initial)
        history.Perform delta |> ignore

    let mutable current = initial

    member x.Update(values : plist<'a>) =
        let delta = plist.ComputeDeltas(current, values)
        history.Perform delta |> ignore
        current <- values

    interface alist<'a> with
        member x.IsConstant = false
        member x.Content = history :> IMod<_>
        member x.GetReader() = history.NewReader()

type ResetMapList<'k, 'v>(initial : plist<'k>, create : Index -> 'k -> 'v, update : 'v * 'k -> unit) =
    
    let history = History PList.trace
    let cache = Dict<Index, ref<'k> * 'v>()

    let mutable current = PList.empty

    let update (keys : plist<'k>) =
        let keyDeltas = plist.ComputeDeltas(current, keys)

        let valueDeltas =
            keyDeltas |> PDeltaList.choose (fun i op ->
                match op with
                    | Set k ->
                        let mutable isNew = false
                        let r, v = 
                            cache.GetOrCreate(i, fun _ ->
                                isNew <- true
                                ref k, create i k
                            )

                        if isNew then
                            Some (Set v)
                        else
                            r := k
                            None

                    | Remove ->
                        match cache.TryRemove i with
                            | (true, (_,v)) ->
                                Some Remove
                            | _ ->
                                None
            )

        current <- keys
        history.Perform valueDeltas |> ignore
        for (r, v) in cache.Values do
            update(v, !r)       

    do update initial

    member x.Update(keys : plist<'k>) =
        update keys

    override x.ToString() =
        current.ToString()

    interface alist<'v> with
        member x.IsConstant = false
        member x.Content = history :> IMod<_>
        member x.GetReader() = history.NewReader()

type ResetMapOption<'a, 'b>(initial : Option<'a>, create : 'a -> 'b, update : 'b * 'a -> unit) =
    inherit Mod.AbstractMod<Option<'b>>()
    let b = ResetMod<Option<'b>>(initial |> Option.map create)

    member x.Update(v : Option<'a>) =
        match b.GetValue(), v with
            | Some _, None -> b.Update(None)
            | None , None -> ()
            | None, Some v -> b.Update(create v |> Some)
            | Some o, Some n ->
                if not (System.Object.ReferenceEquals(o,n)) then
                    update (o,n)
            
    interface IUpdatable<Option<'a>> with
        member x.Update v = x.Update v

    override x.Compute(token) =
        b.GetValue(token)

type ResetMap<'k, 'v>(initial : hmap<'k, 'v>) =
    let history = History HMap.trace
    do 
        let delta = HMap.computeDelta HMap.empty initial
        history.Perform delta |> ignore

    let mutable current = initial

    member x.Update(values : hmap<'k, 'v>) =
        let delta = HMap.computeDelta current values
        history.Perform delta |> ignore
        current <- values

    interface amap<'k, 'v> with
        member x.IsConstant = false
        member x.Content = history :> IMod<_>
        member x.GetReader() = history.NewReader()

type ResetMapMap<'a, 'b, 'v>(initial : hmap<'a, 'b>, create : 'a -> 'b -> 'v, update : 'v * 'b -> unit) =

    let history = History HMap.trace
    let cache = Dict<'a, ref<'b> * 'v>()

    let mutable current = HMap.empty

    let update (keys : hmap<'a, 'b>) =
        let keyDeltas = HMap.computeDelta current keys

        let valueDeltas =
            keyDeltas |> HMap.choose (fun i op ->
                match op with
                    | Set k ->
                        let mutable isNew = false
                        let r, v = 
                            cache.GetOrCreate(i, fun _ ->
                                isNew <- true
                                ref k, create i k
                            )

                        if isNew then
                            Some (Set v)
                        else
                            r := k
                            None

                    | Remove ->
                        match cache.TryRemove i with
                            | (true, (_,v)) ->
                                Some Remove
                            | _ ->
                                None
            )

        current <- keys
        history.Perform valueDeltas |> ignore
        for (r, v) in cache.Values do
            update(v, !r)       

    do update initial

    member x.Update(keys : hmap<'a, 'b>) =
        update keys

    override x.ToString() =
        current.ToString()

    interface amap<'a, 'v> with
        member x.IsConstant = false
        member x.Content = history :> IMod<_>
        member x.GetReader() = history.NewReader()



[<AbstractClass; Sealed>]
type ResetMod private() =
    static member Create(v : 'a) = ResetMod<'a>(v)
    static member Update(m : ResetMod<'a>, v : 'a) = m.Update v


type MSet<'a>(initial : hset<'a>) =
    let history = History HRefSet.traceNoRefCount
    do initial |> Seq.map Add |> HDeltaSet.ofSeq |> history.Perform |> ignore

    let mutable current = initial

    member x.Update(values : hset<'a>) =
        let ops = HSet.computeDelta current values
        current <- values
        history.Perform ops |> ignore

    override x.ToString() =
        current.ToString()
        
    interface IUpdatable<hset<'a>> with
        member x.Update v = x.Update v

    interface aset<'a> with
        member x.IsConstant = false
        member x.GetReader() = history.NewReader()
        member x.Content = history :> IMod<_>

type MSet<'k, 'm, 'v>(getId : 'k -> obj, initial : hset<'k>, create : 'k -> 'm, update : 'm * 'k -> unit, view : 'm -> 'v) =
    let history = History HRefSet.traceNoRefCount
    let cache = Dict<obj, ref<'k> * 'm>()

    let mutable current = HSet.empty

    let update (keys : hset<'k>) =
        let keyDeltas = HSet.computeDelta current keys

        
        let valueDeltas =
            keyDeltas |> HDeltaSet.choose (fun d ->
                match d with
                    | Add(_,k) ->
                        let mutable isNew = false
                        let r, v = 
                            cache.GetOrCreate(getId k, fun _ ->
                                isNew <- true
                                ref k, create k
                            )

                        if isNew then
                            Some (Add (view v))
                        else
                            r := k
                            None

                    | Rem(_,k) ->
                        match cache.TryRemove k with
                            | (true, (_,v)) ->
                                Some (Rem (view v))
                            | _ ->
                                None
            )



        current <- keys
        history.Perform valueDeltas |> ignore
        for (r, v) in cache.Values do
            update(v, !r)
        
    do update initial

    member x.Update(keys : hset<'k>) =
        update keys

    override x.ToString() =
        current.ToString()
        
    interface IUpdatable<hset<'k>> with
        member x.Update v = x.Update v

    interface aset<'v> with
        member x.IsConstant = false
        member x.GetReader() = history.NewReader()
        member x.Content = history :> IMod<_>

[<AbstractClass; Sealed>]
type MSet private() =
    static member Create(getId : 'k -> obj, initial : hset<'k>, create : 'k -> 'm, update : 'm * 'k -> unit, view : 'm -> 'v) =
        MSet<'k, 'm, 'v>(getId, initial, create, update, view)

    static member Update(m : MSet<'k, 'm, 'v>, v : hset<'k>) =
        m.Update(v)

    static member Create(initial : hset<'k>) =
        MSet<'k>(initial)

    static member Update(m : MSet<'k>, v : hset<'k>) =
        m.Update(v)



type MList<'a>(initial : plist<'a>) =
    let history = History PList.trace
    do 
        let delta = plist.ComputeDeltas(PList.empty, initial)
        history.Perform delta |> ignore

    let mutable current = initial

    member x.Update(values : plist<'a>) =
        let delta = plist.ComputeDeltas(current, values)
        history.Perform delta |> ignore
        current <- values

    interface alist<'a> with
        member x.IsConstant = false
        member x.Content = history :> IMod<_>
        member x.GetReader() = history.NewReader()

type MList<'k, 'm, 'v>(initial : plist<'k>, create : 'k -> 'm, update : 'm * 'k -> unit, view : 'm -> 'v) =
    
    let history = History PList.trace
    let cache = Dict<Index, ref<'k> * 'm>()

    let mutable current = PList.empty

    let update (keys : plist<'k>) =
        let keyDeltas = plist.ComputeDeltas(current, keys)

        let valueDeltas =
            keyDeltas |> PDeltaList.choose (fun i op ->
                match op with
                    | Set k ->
                        let mutable isNew = false
                        let r, v = 
                            cache.GetOrCreate(i, fun _ ->
                                isNew <- true
                                ref k, create k
                            )

                        if isNew then
                            Some (Set (view v))
                        else
                            r := k
                            None

                    | Remove ->
                        match cache.TryRemove i with
                            | (true, (_,v)) ->
                                Some Remove
                            | _ ->
                                None
            )

        current <- keys
        history.Perform valueDeltas |> ignore
        for (r, v) in cache.Values do
            update(v, !r)       

    do update initial

    member x.Update(keys : plist<'k>) =
        update keys

    override x.ToString() =
        current.ToString()

    interface alist<'v> with
        member x.IsConstant = false
        member x.Content = history :> IMod<_>
        member x.GetReader() = history.NewReader()

[<AbstractClass; Sealed>]
type MList private() =
    static member Create(initial : plist<'k>, create : 'k -> 'm, update : 'm * 'k -> unit, view : 'm -> 'v) =
        MList<'k, 'm, 'v>(initial, create, update, view)

    static member Update(m : MList<'k, 'm, 'v>, v : plist<'k>) =
        m.Update(v)

    static member Create(initial : plist<'k>) =
        MList<'k>(initial)

    static member Update(m : MList<'k>, v : plist<'k>) =
        m.Update(v)


type MMap<'k, 'v>(initial : hmap<'k, 'v>) =
    let history = History HMap.trace
    do 
        let delta = HMap.computeDelta HMap.empty initial
        history.Perform delta |> ignore

    let mutable current = initial

    member x.Update(values : hmap<'k, 'v>) =
        let delta = HMap.computeDelta current values
        history.Perform delta |> ignore
        current <- values

    interface amap<'k, 'v> with
        member x.IsConstant = false
        member x.Content = history :> IMod<_>
        member x.GetReader() = history.NewReader()

type MMap<'a, 'b, 'm, 'v>(initial : hmap<'a, 'b>, create : 'b -> 'm, update : 'm * 'b -> unit, view : 'm -> 'v) =

    let history = History HMap.trace
    let cache = Dict<'a, ref<'b> * 'm>()

    let mutable current = HMap.empty

    let update (keys : hmap<'a, 'b>) =
        let keyDeltas = HMap.computeDelta current keys

        let valueDeltas =
            keyDeltas |> HMap.choose (fun i op ->
                match op with
                    | Set k ->
                        let mutable isNew = false
                        let r, v = 
                            cache.GetOrCreate(i, fun _ ->
                                isNew <- true
                                ref k, create k
                            )

                        if isNew then
                            Some (Set (view v))
                        else
                            r := k
                            None

                    | Remove ->
                        match cache.TryRemove i with
                            | (true, (_,v)) ->
                                Some Remove
                            | _ ->
                                None
            )

        current <- keys
        history.Perform valueDeltas |> ignore
        for (r, v) in cache.Values do
            update(v, !r)       

    do update initial

    member x.Update(keys : hmap<'a, 'b>) =
        update keys

    override x.ToString() =
        current.ToString()

    interface amap<'a, 'v> with
        member x.IsConstant = false
        member x.Content = history :> IMod<_>
        member x.GetReader() = history.NewReader()

[<AbstractClass; Sealed>]
type MMap private() =
    static member Create(initial : hmap<'a, 'b>, create : 'b -> 'm, update : 'm * 'b -> unit, view : 'm -> 'v) =
        MMap<'a, 'b, 'm, 'v>(initial, create, update, view)

    static member Update(m : MMap<'a, 'b, 'm, 'v>, v : hmap<'a, 'b>) =
        m.Update(v)

    static member Create(initial : hmap<'a, 'b>) =
        MMap<'a, 'b>(initial)

    static member Update(m : MMap<'a, 'b>, v : hmap<'a, 'b>) =
        m.Update(v)



type MOption<'a>(initial : Option<'a>) =
    inherit ResetMod<Option<'a>>(initial)

type MOption<'a, 'b, 'v>(initial : Option<'a>, create : 'a -> 'b, update : 'b * 'a -> unit, view : 'b -> 'v) =
    inherit Mod.AbstractMod<Option<'v>>()
    let b = ResetMod<Option<'b>>(initial |> Option.map create)

    member x.Update(v : Option<'a>) =
        match b.GetValue(), v with
            | Some _, None -> b.Update(None)
            | None , None -> ()
            | None, Some v -> b.Update(create v |> Some)
            | Some o, Some n ->
                if not (System.Object.ReferenceEquals(o,n)) then
                    update (o,n)
            
    interface IUpdatable<Option<'a>> with
        member x.Update v = x.Update v

    override x.Compute(token) =
        b.GetValue(token) |> Option.map view

[<AbstractClass; Sealed>]
type MOption private() =
    static member Create(initial : Option<'a>, create : 'a -> 'm, update : 'm * 'a -> unit, view : 'm -> 'v) =
        MOption<'a, 'm, 'v>(initial, create, update, view)

    static member Update(m : MOption<'a, 'm, 'v>, v : Option<'a>) =
        m.Update(v)

    static member Create(initial : Option<'a>) =
        MOption<'a>(initial)

    static member Update(m : MOption<'a>, v : Option<'a>) =
        m.Update(v)



[<DomainType>]
type Tup<'a, 'b> = { l : 'a; r : 'b }

[<DomainType>]
type Thing<'a> = { value : 'a; name : string }


[<AutoOpen>]
module Mutable =
    [<AbstractClass>]
    type MThing<'ma>() =
        abstract member value: 'ma
        abstract member name : IMod<string>

    type private MThingV<'a>(__initial : Thing<'a>) =
        inherit MThing<IMod<'a>>()

        let mutable __current = __initial

        let _value = ResetMod.Create(__initial.value)
        let _name = ResetMod.Create(__initial.name)

        override x.value = _value :> IMod<_>
        override x.name = _name :> IMod<_>

        member x.Update(v : Thing<'a>) =
            if not (System.Object.ReferenceEquals(__current, v)) then
                __current <- v
                _value.Update(v.value)
                _name.Update(v.name)

        interface IUpdatable<Thing<'a>> with
            member x.Update v = x.Update v

    type private MThingD<'a, 'ma>(__initial : Thing<'a>, init : 'a -> 'ma, update : 'ma * 'a -> unit) =
        inherit MThing<'ma>()

        let mutable __current = __initial

        let _value = init(__initial.value)
        let _name = ResetMod.Create(__initial.name)

        override x.value = _value
        override x.name = _name :> IMod<_>

        member x.Update(v : Thing<'a>) =
            if not (System.Object.ReferenceEquals(__current, v)) then
                __current <- v
                update(_value, v.value)
                _name.Update(v.name)

        interface IUpdatable<Thing<'a>> with
            member x.Update v = x.Update v

    [<AbstractClass; Sealed; Extension>]
    type MThing private() =
        static member CreateV(v : Thing<'a>) =
            MThingV<'a>(v) :> MThing<_>

        static member CreateD(v : Thing<'a>, init : 'a -> 'ma, update : 'ma * 'a -> unit) =
            MThingD(v, init, update) :> MThing<_>
        
        [<Extension>]
        static member Update(m : MThing<'ma>, v : Thing<'a>) =
            match m :> obj with    
                | :? IUpdatable<Thing<'a>> as m -> m.Update(v)
                | _ -> failwith "asdasds"


    [<AbstractClass>]
    type MTup<'ma, 'mb>() =
        abstract member l: 'ma
        abstract member r : 'mb

    type private MTupVV<'a, 'b>(__initial : Tup<'a, 'b>) =
        inherit MTup<IMod<'a>, IMod<'b>>()

        let mutable __current = __initial

        let _l = ResetMod.Create(__initial.l)
        let _r = ResetMod.Create(__initial.r)

        override x.l = _l :> IMod<_>
        override x.r = _r :> IMod<_>

        member x.Update(v : Tup<'a, 'b>) =
            if not (System.Object.ReferenceEquals(__current, v)) then
                __current <- v
                _l.Update(v.l)
                _r.Update(v.r)

        interface IUpdatable<Tup<'a, 'b>> with
            member x.Update v = x.Update v

    type private MTupVD<'a, 'b, 'mb>(__initial : Tup<'a, 'b>, __initb : 'b -> 'mb, __updateb : 'mb * 'b -> unit) =
        inherit MTup<IMod<'a>, 'mb>()

        let mutable __current = __initial

        let _l = ResetMod.Create(__initial.l)
        let _r = __initb(__initial.r)

        override x.l = _l :> IMod<_>
        override x.r = _r

        member x.Update(v : Tup<'a, 'b>) =
            if not (System.Object.ReferenceEquals(__current, v)) then
                __current <- v
                _l.Update(v.l)
                __updateb(_r, v.r)

        interface IUpdatable<Tup<'a, 'b>> with
            member x.Update v = x.Update v

    type private MTupDV<'a, 'ma, 'b>(__initial : Tup<'a, 'b>, __inita : 'a -> 'ma, __updatea : 'ma * 'a -> unit) =
        inherit MTup<'ma, IMod<'b>>()

        let mutable __current = __initial

        let _l = __inita(__initial.l)
        let _r = ResetMod.Create(__initial.r)

        override x.l = _l
        override x.r = _r :> IMod<_>

        member x.Update(v : Tup<'a, 'b>) =
            if not (System.Object.ReferenceEquals(__current, v)) then
                __current <- v
                __updatea(_l, v.l)
                _r.Update(v.r)

        interface IUpdatable<Tup<'a, 'b>> with
            member x.Update v = x.Update v

    type private MTupDD<'a, 'ma, 'b, 'mb>(__initial : Tup<'a, 'b>, __inita : 'a -> 'ma, __updatea : 'ma * 'a -> unit, __initb : 'b -> 'mb, __updateb : 'mb * 'b -> unit) =
        inherit MTup<'ma, 'mb>()

        let mutable __current = __initial

        let _l = __inita(__initial.l)
        let _r = __initb(__initial.r)

        override x.l = _l
        override x.r = _r

        member x.Update(v : Tup<'a, 'b>) =
            if not (System.Object.ReferenceEquals(__current, v)) then
                __current <- v
                __updatea(_l, v.l)
                __updateb(_r, v.r)

        interface IUpdatable<Tup<'a, 'b>> with
            member x.Update v = x.Update v

    [<AbstractClass; Sealed; Extension>]
    type MTup private() =
        static member CreateVV(v : Tup<'a, 'b>) =
            MTupVV(v) :> MTup<_,_>

        static member CreateVD(v, ib, ub) =
            MTupVD(v, ib, ub) :> MTup<_,_>

        static member CreateDV(v, ia, ua) =
            MTupDV(v, ia, ua) :> MTup<_,_>
        
        static member CreateDD(v, ia, ua, ib, ub) =
            MTupDD(v, ia, ua, ib, ub) :> MTup<_,_>

        [<Extension>]
        static member Update(m : MTup<'ma, 'mb>, v : Tup<'a, 'b>) =
            match m :> obj with    
                | :? IUpdatable<Tup<'a, 'b>> as m -> m.Update(v)
                | _ -> failwith "asdasds"
