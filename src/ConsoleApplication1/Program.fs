// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.
open Aardvark.Base
open Aardvark.Base.Incremental
open Sepp

[<EntryPoint>]
let main argv = 
    let o0 =
        { 
            _id = 0L
            trafo = Trafo3d.Scale 10.0
            model = "object 0"
        }

    let o1 = 
        {
            _id = 0L
            trafo = Trafo3d.Translation(1.0, 0.0, 0.0)
            model = "object 1"
        }
    
    let state =
        {
            _id = 0L
            viewTrafo = Trafo3d.Identity
            objects = PSet.ofList [o0]
        }



    let ms = state.ToMod()
    
    ms.mobjects |> ASet.unsafeRegisterCallbackKeepDisposable (fun o -> 
        o |> List.iter (fun o ->
            match o with
                | Add v -> printfn "Add %A" (Mod.force v.mmodel)
                | Rem v -> printfn "Rem %A" (Mod.force v.mmodel)
        )
    ) |> ignore

    transact (fun () ->
        ms.Apply({ state with objects = PSet.add o1 state.objects })
    )

    transact (fun () ->
        ms.Apply({ state with objects = PSet.remove o1 state.objects })
    )




    0 // return an integer exit code
