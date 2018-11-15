open System

open Demo
open Aardvark.Base
open Aardvark.Base.Incremental


[<EntryPoint>]
let main argv =

    let thing = { value = 1 }
    let mthing = MThing.Create(thing)
    let adaptiveCode = 
        mthing.value |> Mod.map (sprintf "%A") 

    printfn "%A" (Mod.force adaptiveCode)

    let newThing = { value = 10 }
    transact (fun _ -> mthing.Update(newThing))
    printfn "%A" (Mod.force adaptiveCode)

    let example =
        {
            value = 0
            hset  = HSet.empty
            hsetM = HSet.empty
            plist = PList.empty
            map   = HMap.empty
            list   = []
            plainArray = [||]
        }

    let mExample = MExample.Create example
    let value : IMod<int>    = mExample.value
    let hset  : aset<int>    = mExample.hset
    let hsetM : aset<MThing> = mExample.hsetM
    let plist : alist<MThing> = mExample.plist
    let map   : amap<int,MThing>  = mExample.map
    let list  : IMod<list<Thing>> = mExample.list
    let plainArray : IMod<array<int>> = mExample.plainArray

    0 
