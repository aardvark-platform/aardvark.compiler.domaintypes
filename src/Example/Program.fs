// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.
open Aardvark.Base
open Aardvark.Base.Incremental
open DomainModel
open Aardvark.Compiler.DomainTypes


let test () =
    let mo =     
        {
            fileName    = "sadsad"
            bounds      = Box3d.Unit
        }

    let o = 
        { 
            name = "obj"
            trafo = Trafo3d.Identity
            model = mo 
        }

    let state = 
        { 
            past = None
            primary = Some o
            viewTrafo = Trafo3d.Identity
            objects = HSet.empty
            test = PList.empty
        }

    let mstate = MState.Create state


    mstate.objects |> ASet.unsafeRegisterCallbackKeepDisposable (fun deltas ->
        printfn "delta: %A" deltas

        for d in deltas do
            match d with
                | Add(_,v) -> 
                    v.trafo |> Mod.unsafeRegisterCallbackKeepDisposable (fun t -> 
                        printfn "trafo(%s): %A" (Mod.force v.name) t.Forward.Det
                    ) |> ignore
                | _ -> 
                    ()

    ) |> ignore

    printfn "add obj"
    let state = { state with objects = HSet.add o state.objects }
    transact (fun () -> mstate.Update state )

    printfn "set trafo (scale 3.0)"
    let n = { o with trafo = Trafo3d.Scale 3.0 }
    let state = { state with objects = HSet.remove o (HSet.add n state.objects) }
    transact (fun () -> mstate.Update state )
    let o = n
    
    // TODO: remove missing here
    printfn "change name and set trafo (scale 6.0)"
    let n = { o with name = "hugo"; trafo = Trafo3d.Scale 6.0 }
    let state = { state with objects = HSet.remove o (HSet.add n state.objects) }
    transact (fun () -> mstate.Update state )
    let o = n


    System.Environment.Exit 0
    ()

open System.IO

let testUnion() =
    let u = A(1,HSet.empty)
    let sepp = MMyUnion.Create u

    let test =
        adaptive {
            let! sepp = sepp
            match sepp with
                | MA (x,y) -> return 2
                | MB str -> return 54
                | MC -> return 2
        }

    sepp |> Mod.unsafeRegisterCallbackKeepDisposable (fun c -> printfn "case changed: %A" c) |> ignore

    printfn "update a"
    let u = A(2,HSet.empty)
    transact (fun () -> sepp.Update u)

    printfn "change to b"
    let u = B "asdsad"
    transact (fun () -> sepp.Update u)

    printfn "change to c"
    let u = C
    transact (fun () -> sepp.Update u)
    
    System.Environment.Exit 0

[<EntryPoint>]
let main argv =
    //test()
    //testUnion()


    let fsProjPath = @"C:\Users\Schorsch\Development\aardvark-compiler-domaintypes\src\Example\Example.fsproj"

    let files =
        [
            "Tests.fs"
            "DomainModel.fs"
            "Program.fs"
        ]

    let references =
        Set.ofList [
            "System.Drawing.dll"
            "Aardvark.Compiler.DomainTypes.exe"
            "Aardvark.Base.dll"
            "Aardvark.Base.Essentials.dll"
            "Aardvark.Base.TypeProviders.dll"
            "Aardvark.Base.FSharp.dll"
            "Aardvark.Base.Incremental.dll"
            "DevILSharp.dll"

            
            "FSharp.Compiler.Service.MSBuild.v12.dll"
            "FSharp.Compiler.Service.dll"
            "FSharp.Compiler.Service.ProjectCracker.dll"
            "System.Collections.Immutable.dll"
            "System.Reactive.Core.dll"
            "System.Reactive.Interfaces.dll"
            "System.Reactive.Linq.dll"
            "System.Reflection.Metadata.dll"
        ]

    let test = Preprocessing.run fsProjPath references files |> Option.get

    for (f,content) in Map.toSeq test do
        let path = System.IO.Path.ChangeExtension(f, ".g.fs")

        let old = 
            if File.Exists path then File.readAllText path
            else ""

        if old <> content then
            File.writeAllText path content
            printfn "%s:" f
            printfn "%s" content


    0
