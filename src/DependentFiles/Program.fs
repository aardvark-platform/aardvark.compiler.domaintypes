// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.
open Aardvark.Base
open Aardvark.Base.Incremental
open Sepp


type pset<'a> = PersistentHashSet<'a>
module PSet = PersistentHashSet

[<EntryPoint>]
let main argv = 
    let state =
        {
            _id = 0L
            a = 1
            b = 1.0
            objects = PSet.empty
        }

    let ms = state.ToMod()
    printfn "%A" (Mod.force ms.mb)

    transact (fun () ->
        ms.Apply({ state with a = 4 })
    )
    printfn "%A" ms.mb.OutOfDate

    printfn "%A" (Mod.force ms.ma)
    0 // return an integer exit code
