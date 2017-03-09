namespace Aardvark.Compiler.DomainTypes

open System
open System.IO
open Microsoft.Build.Framework
open Microsoft.Build.Utilities

module CodeGenerator =
    type Result<'a> = 
        | Success of 'a
        | Error of string

    type CodeGenState = 
        { 
            result : System.Text.StringBuilder
            indent : string 
            warnings : list<string>
        }
        
    type CodeGen<'a> = { generate : CodeGenState -> Result<CodeGenState * 'a> }

    type CodeGenBuilder() =
        member x.Bind(m : CodeGen<'a>, f : 'a -> CodeGen<'b>) =
            { generate = fun s ->
                match m.generate s with
                    | Success(s,v) -> f(v).generate s
                    | Error e -> Error e
            }

        member x.Return(v : 'a) =
            { generate = fun s ->
                Success(s, v)
            }

        member x.Zero() =
            { generate = fun s ->
                Success(s, ())
            }

        member x.Delay(f : unit -> CodeGen<'a>) =
            { generate = fun s ->
                f().generate s
            }

        member x.For(e : seq<'a>, f : 'a -> CodeGen<unit>) =
            { generate = fun s ->
                use e = e.GetEnumerator()
                let rec run(s) =
                    if e.MoveNext() then
                        match f(e.Current).generate s with
                            | Success(s, ()) -> run s
                            | Error e -> Error e
                    else
                        Success(s, ())
                run s
            }

        member x.While(guard : unit -> bool, body : CodeGen<unit>) =
            { generate = fun s ->
                let rec run s =
                    if guard() then
                        match body.generate s with
                            | Success(s,()) -> run s
                            | Error e -> Error e
                    else
                        Success(s, ())
                run s
            }
    
        member x.Combine(l : CodeGen<unit>, r : CodeGen<'a>) =
            { generate = fun s ->
                match l.generate s with
                    | Success(s,()) ->
                        r.generate s
                    | Error e ->
                        Error e
            }
            
    let codegen = CodeGenBuilder()


    [<AutoOpen>]
    module CodeGen =
        let line fmt = 
            let inner (str : string) =
                { generate = fun s ->
                    s.result.AppendLine(s.indent + str) |> ignore
                    Success(s,())
                }
            Printf.kprintf inner fmt

        let error fmt =
            Printf.kprintf (fun str ->
                { generate = fun s ->
                    Error str
                }
            ) fmt

        let warn fmt =
            Printf.kprintf (fun str ->
                { generate = fun s ->
                    Success({s with warnings = s.warnings @ [str] }, ())
                }
            ) fmt

        let push = { generate = fun s -> Success({ s with indent = s.indent + "    " }, ()) }
        let pop = { generate = fun s -> Success({ s with indent = s.indent.Substring 4 }, ()) }
            

        type InnerBuilder(str : string) =
            inherit CodeGenBuilder()

            member x.Run(g : CodeGen<'a>) =
                codegen {
                    do! line "%s" str
                    do! push
                    let! res = g
                    do! pop
                    return res
                }

        let inline scope fmt = Printf.kprintf InnerBuilder fmt

        let run (g : CodeGen<unit>) =
            let res = 
                g.generate {
                    result = System.Text.StringBuilder()
                    indent = ""
                    warnings = []
                }
            match res with
                | Success(s,_) -> s.result.ToString()
                | Error e -> failwith e

module Preprocessing =
    open CodeGenerator
    open Microsoft.FSharp.Compiler
    open Microsoft.FSharp.Compiler.SourceCodeServices.ProjectCrackerTool
    open Microsoft.FSharp.Compiler.SourceCodeServices
    
    let checker = FSharpChecker.Create(keepAssemblyContents = true)

    let domainAttName = "Aardvark.Base.Incremental.DomainTypeAttribute"

    module FSharpAttribute =
        let isDomainAttribute (a : FSharpAttribute) =
            a.AttributeType.FullName = domainAttName

    module FSharpType = 
        let isDomainType (t : FSharpType) =
            if t.HasTypeDefinition then
                t.TypeDefinition.Attributes |> Seq.exists FSharpAttribute.isDomainAttribute
            else
                false

        let mutableName (e : FSharpType) =
            if e.HasTypeDefinition  then
                "M" + e.TypeDefinition.DisplayName
            else
                failwithf "[Domain] no definition for %A" e 

        let immutableName (e : FSharpType) =
            if e.HasTypeDefinition  then
                let def = e.TypeDefinition
                match def.TryFullName with
                    | Some name -> name
                    | None -> def.DisplayName
            else
                failwithf "[Domain] no definition for %A" e 

    module FSharpEntity =
        let immutableName (e : FSharpEntity) =
            e.FullName 

        let mutableName (e : FSharpEntity) =
            "M" + e.DisplayName 

        let mutableNameSpace (e : FSharpEntity) =
            let name = e.FullName
            let idx = name.LastIndexOf '.'
            name.Substring(0, idx)

    let rec findDomainTypes (e : FSharpImplementationFileDeclaration) =
        seq {
            match e with
                | Entity(e, children) ->
                    if e.Attributes |> Seq.exists FSharpAttribute.isDomainAttribute then
                        yield e

                    for c in children do
                        yield! findDomainTypes c
                | _ ->
                    ()
        }

    [<AutoOpen>]
    module Patterns =
        let (|DomainType|_|) (t : FSharpType) =
            if FSharpType.isDomainType t then Some t
            else None

        let (|Generic|_|) (t : FSharpType) =
            if t.HasTypeDefinition then
                let d = t.TypeDefinition
                let name = t.TypeDefinition.DisplayName
                let targs = t.GenericArguments |> Seq.toList
                
                Some(name, targs)
            else
                None
            

        let (|HSet|_|) (t : FSharpType) =
            match t with
                | Generic("hset", [a]) -> Some a
                | _ -> None
                
        let (|PList|_|) (t : FSharpType) =
            match t with
                | Generic("plist", [a]) -> Some a
                | _ -> None

        let tryGetUniqueField (t : FSharpType) =
            if t.HasTypeDefinition then
                let def = t.TypeDefinition

                let found = 
                    if def.IsFSharpRecord then
                        def.FSharpFields |> Seq.tryPick (fun f ->
                            let isPrimaryKey = f.PropertyAttributes |> Seq.exists (fun a -> a.AttributeType.FullName = "Aardvark.Base.Incremental.PrimaryKeyAttribute")
                            if isPrimaryKey then
                                Some (f.DisplayName)
                            else
                                None
                        )
                    else
                        None

                match found with
                    | Some f -> Some f
                    | None -> 
                        def.MembersFunctionsAndValues |> Seq.tryPick (fun mfv ->
                            let isPrimaryKey = mfv.Attributes |> Seq.exists (fun a -> a.AttributeType.FullName = "Aardvark.Base.Incremental.PrimaryKeyAttribute")
                            if isPrimaryKey then
                                Some (mfv.DisplayName)
                            else
                                None
                        )
            else
                None


    type Adapter =
        {
            aType : Option<string>
            aInit : string -> string
        }

    let generateAdapter (t : FSharpType) =
        match t with
            | PList (DomainType t) ->
                let tName = FSharpType.mutableName t
                {
                    aType = Some "alist<_>"
                    aInit = fun fName -> sprintf "ResetMapList(%s, (fun _ -> %s.Create), fun (m,i) -> m.Update(i))"  fName tName
                }

            | PList t ->
                {
                    aType = Some "alist<_>"
                    aInit = sprintf "ResetList(%s)"
                }

            | HSet (DomainType t) ->
                let tName = FSharpType.mutableName t
                let getKey = 
                    match tryGetUniqueField t with
                        | Some field -> sprintf "(fun v -> v.%s :> obj)" field
                        | None -> "unbox"
                {
                    aType = Some "aset<_>"
                    aInit = fun fName -> sprintf "ResetMapSet(%s, %s, %s.Create, fun (m,i) -> m.Update(i))" getKey fName tName
                }

            | HSet t ->
                let tName = FSharpType.mutableName t
                {
                    aType = Some "aset<_>"
                    aInit = fun fName -> sprintf "ResetSet(%s)" fName
                }
                       
            | DomainType t ->
                let mName = FSharpType.mutableName t
                {
                    aType = None
                    aInit = sprintf "%s.Create(%s)" mName
                }

            | t ->
                {
                    aType = Some "IMod<_>"
                    aInit = sprintf "ResetMod(%s)"
                }
                
    let generateMutableModels (l : list<FSharpEntity>) =
        codegen {
            for e in l do
                let immutableName = FSharpEntity.immutableName e
                let mutableName = FSharpEntity.mutableName e
                let targs = e.GenericParameters
                if targs.Count > 0 then
                    do! error "cannot compile generic domain type %s" e.DisplayName

                do! line "namespace %s" (FSharpEntity.mutableNameSpace e)
                do! line "open System"
                do! line "open Aardvark.Base.Incremental"
                do! line ""

                if e.IsFSharpRecord then
                    let annotatedFields =
                        e.FSharpFields
                            |> Seq.toList
                            |> List.map (fun f ->
                                let adapter = generateAdapter f.FieldType
                                let inputName = sprintf "__initial.%s" f.DisplayName
                                let init = sprintf "let _%s = %s" f.DisplayName (adapter.aInit inputName)

                                let access =
                                    match adapter.aType with
                                        | Some t -> sprintf "_%s :> %s" f.DisplayName t
                                        | None -> sprintf "_%s" f.DisplayName

                                f.DisplayName, init, access
                        )
                    
                    let typeDef = scope "type %s private(__initial : %s) =" mutableName immutableName
                    do! typeDef {
                        do! line "let mutable __current = __initial"

                        for (_, init, _) in annotatedFields do
                            do! line "%s" init
                        
                        do! line ""

                        for (fname, _, access) in annotatedFields do
                            do! line "member x.%s = %s" fname access
                    
                        do! line ""

                        let apply = scope "member x.Update(__model : %s) =" immutableName
                        do! apply {
                            do! line "if not (Object.ReferenceEquals(__model, __current)) then"
                            do! push

                            do! line "__current <- __model"
                
                            for f in e.FSharpFields do
                                let fName = f.DisplayName
                                do! line "_%s.Update(__model.%s)" fName fName
                            do! pop
                        }

                        do! line "static member Create(initial) = %s(initial)" mutableName

                    }

                elif e.IsFSharpUnion then
                    let cases = e.UnionCases

//                    do! line "type %s =" mutableName
//                    do! push
//                    for c in cases do
//                        let caseName = "M" + c.Name
//                        let args = 
//                            c.UnionCaseFields
//                                |> Seq.toList
//                                |> List.map (fun f ->
//                                    match f.FieldType with
//                                        | DomainType t -> f.Name, FSharpType.mutableName t
//                                        | _ -> 
//                                )
//                        ()
//
//                    do! pop
                    do! error "cannot compile union types atm."
                    ()

                else
                    failwithf "[Domain] bad domain type: %A" e

        }

    let runInternal (fsProjPath : string) =
        async {
            let sw = System.Diagnostics.Stopwatch()
            sw.Start()
            let options = ProjectCracker.GetProjectOptionsFromProjectFile(fsProjPath)
            sw.Stop()
            printfn "%.1fs" sw.Elapsed.TotalSeconds

            let! res = checker.ParseAndCheckProject(options)
            if res.HasCriticalErrors then
                return None

            else
                let domainTypes = 
                    res.AssemblyContents.ImplementationFiles |> List.choose (fun f ->
                        let domainTypes = f.Declarations |> Seq.collect findDomainTypes |> Seq.toList
                        match domainTypes with
                            | [] -> None
                            | _ -> Some (f.FileName, domainTypes)
                    )
                    |> Map.ofList

                let code = domainTypes |> Map.map (fun _ c -> generateMutableModels c |> run)
                return Some code
        }

    let run (fsProjPath : string) =
        let test = runInternal fsProjPath |> Async.RunSynchronously
        test

type Preprocess() =
    inherit Task()

    let mutable item = ""
    let mutable current = ""
    let mutable results = [||]

    override x.Execute() =
        let all = current.Split([|';'|], StringSplitOptions.RemoveEmptyEntries) |> Set.ofArray

        if Set.contains item all then
            let fileName = Path.GetFileNameWithoutExtension item
            let domFile = fileName + ".g.fs"
            let res = failwith "" //Preprocessing.run x.Log (Path.Combine(System.Environment.CurrentDirectory, item)) (Path.Combine(System.Environment.CurrentDirectory,domFile)) 
            if res then
                results <- [|item; domFile|]
                true
            else
                results <- [|item|]
                false
        else
            results <- [|item|]
            true

    [<Required>]
    member x.Item
        with get() = item
        and set i = item <- i

    [<Required>]
    member x.Current
        with get() = current
        and set c = current <- c

    [<Output>]
    member x.Results
        with get() = results
        and set r = results <- r