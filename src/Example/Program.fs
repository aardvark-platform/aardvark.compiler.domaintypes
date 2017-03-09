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
            primary = o
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



type MyUnion =
    | A of int * hset<int>
    | B of string

// generated
[<AbstractClass; System.Runtime.CompilerServices.Extension; StructuredFormatDisplay("{AsString}")>]
type MMyUnion() =
    
    abstract member TryUpdate : MyUnion -> bool
    abstract member AsString : string
        
    static member private CreateValue(v : MyUnion) =
        match v with
            | A(item1, item2) -> MA(v, item1, item2) :> MMyUnion
            | B(item1) -> MB(v, item1) :> MMyUnion

    static member Create(v : MyUnion) =
        ResetMod(MMyUnion.CreateValue v) :> IMod<_>

    [<System.Runtime.CompilerServices.Extension>]
    static member Update(m : IMod<MMyUnion>, v : MyUnion) =
        let m = unbox<ResetMod<MMyUnion>> m
        if not (m.GetValue().TryUpdate v) then
            m.Update(MMyUnion.CreateValue v)

and private MA(_initial : MyUnion, item1 : int, item2 : hset<int>) =
    inherit MMyUnion()

    let mutable _current = _initial
    let _item1 = ResetMod(item1)
    let _item2 = ResetSet(item2)

    member x.Item1 = _item1 :> IMod<_>
    member x.Item2 = _item2 :> aset<_>

    
    override x.ToString() = _current.ToString()
    override x.AsString = sprintf "%A" _current
    override x.TryUpdate(_model : MyUnion) =
        if System.Object.ReferenceEquals(_current, _model) then
            true
        else
            match _model with
                | A(item1, item2) ->
                    _current <- _model
                    _item1.Update(item1)
                    _item2.Update(item2)
                    true
                | _ ->
                    false

and private MB(_initial : MyUnion, item1 : string) =
    inherit MMyUnion()
    
    let mutable _current = _initial
    let _item1 = ResetMod(item1)

    member x.Item1 = _item1 :> IMod<_>

    override x.ToString() = _current.ToString()
    override x.AsString = sprintf "%A" _current
    override x.TryUpdate(_model : MyUnion) =
        if System.Object.ReferenceEquals(_current, _model) then
            true
        else
            match _model with
                | B(item1) ->
                    _current <- _model
                    _item1.Update(item1)
                    true
                | _ ->
                    false          

[<AutoOpen>]
module MMyUnionPatterns =
    let (|MA|MB|) (m : MMyUnion) =
        match m with
            | :? MA as a -> MA(a.Item1, a.Item2)
            | :? MB as b -> MB(b.Item1)
            | _ -> failwith "impossible"

// end

let testUnion() =
    let u = A(1,HSet.empty)
    let sepp = MMyUnion.Create u

    sepp |> Mod.unsafeRegisterCallbackKeepDisposable (fun c -> printfn "case changed: %A" c) |> ignore

    printfn "update a"
    let u = A(2,HSet.empty)
    transact (fun () -> sepp.Update u)

    printfn "change to b"
    let u = B "asdsad"
    transact (fun () -> sepp.Update u)
    
    System.Environment.Exit 0

[<EntryPoint>]
let main argv =
    //test()
    testUnion()


    let fsProjPath = @"E:\Development\aardvark-compiler-domaintypes\src\Example\Example.fsproj"

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
