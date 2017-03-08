// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.
open Aardvark.Base
open Aardvark.Base.Incremental
open DomainModel
open Aardvark.Compiler.DomainTypes

[<EntryPoint>]
let main argv = 
    
    let mo =     
        {
            fileName    = "sadsad"
            bounds      = Box3d.Unit
        }
    let o = { name = "obj"; trafo = Trafo3d.Identity; model = mo }

    let state = { primary = o; viewTrafo = Trafo3d.Identity; objects = HSet.empty; test = [||] }

    let mstate = MState(state)

    let name = mstate.primary.name

    let a = mstate.viewTrafo.GetValue()
    printfn "%A" a.Forward

    transact (fun () ->
        mstate.Apply({ state with viewTrafo = Trafo3d.Scale(2.0) })
    )
    let b = mstate.viewTrafo.GetValue()
    printfn "%A" b.Forward
    System.Environment.Exit 0


//    [<DomainType>]
//    type Object =
//        {
//            name        : string
//            trafo       : Trafo3d
//            model       : Model
//        }
//
//    [<DomainType>]
//    type State =
//        {
//            primary     : Object
//            viewTrafo   : Trafo3d
//            objects     : hset<Object>
//            test        : array<Object>
//        }


    let test = Preprocessing.run @"C:\Users\Schorsch\Development\aardvark-compiler-domaintypes\src\Example\Example.fsproj"
    printfn "%A" test
    0 // return an integer exit code
