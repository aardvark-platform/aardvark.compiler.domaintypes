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
