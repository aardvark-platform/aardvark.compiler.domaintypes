// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.
open Aardvark.Base
open Aardvark.Base.Incremental
open DomainModel

[<EntryPoint>]
let main argv = 
    let o0 =
        { 
            _id = null
            name = ""
            trafo = Trafo3d.Scale 10.0
            model = { fileName = "obj0.obj"; bounds = Box3d.Unit }
        }

    let o1 = 
        {
            _id = null
            name = ""
            trafo = Trafo3d.Translation(1.0, 0.0, 0.0)
            model = { fileName = "obj1.obj"; bounds = Box3d.Unit }
        }
    
    let state =
        {
            _id = null
            primary = o0
            viewTrafo = Trafo3d.Identity
            objects = PSet.ofList [o0]
            test = [|o0|]
        }


    let scope = ReuseCache()
    let ms = state.ToMod(scope)

    ms.mobjects |> ASet.unsafeRegisterCallbackKeepDisposable (fun o -> 
        o |> List.iter (fun o ->
            match o with
                | Add v -> printfn "Add %A" (Mod.force v.mmodel)
                | Rem v -> printfn "Rem %A" (Mod.force v.mmodel)
        )
    ) |> ignore

    transact (fun () ->
        ms.Apply({ state with objects = PSet.add o1 state.objects }, scope)
    )

    transact (fun () ->
        ms.Apply({ state with objects = PSet.remove o1 state.objects }, scope)
    )




    0 // return an integer exit code
