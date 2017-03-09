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
    open Microsoft.FSharp.Compiler.SourceCodeServices
    
    let checker = FSharpChecker.Create(keepAssemblyContents = true)

    let domainAttName = "Aardvark.Base.Incremental.DomainTypeAttribute"

    let getProjectOptions (fsprojFile : string) (references : list<string>) (files : list<string>) =
        let args =
            [|
                yield "--simpleresolution"
                yield "--noframework"
                yield "--fullpaths"
                yield "--flaterrors"
                yield "--platform:anycpu"

                for r in references do
                    yield "-r:" + r 

                yield! files 
            |]

        checker.GetProjectOptionsFromCommandLineArgs(fsprojFile, args)

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
        
        let private rx = System.Text.RegularExpressions.Regex @"`[0-9]+"

        let rec immutableName (e : FSharpType) =
            if e.HasTypeDefinition  then
                let def = e.TypeDefinition
                let targs = e.GenericArguments |> Seq.toList
                match targs with
                    | [] -> 
                        match def.TryFullName with
                            | Some name -> name
                            | None -> def.DisplayName
                    | _ ->
                        let targs = targs |> List.map immutableName |> String.concat ", " |> sprintf "<%s>"
                        match def.TryFullName with
                            | Some name -> rx.Replace(name, targs) 
                            | None -> def.DisplayName + targs
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

    module FSharpField =
        let treatAsValue (f : FSharpField) =
            f.PropertyAttributes |> Seq.exists (fun a -> a.AttributeType.FullName = "Aardvark.Base.Incremental.TreatAsValueAttribute")

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
                
        let (|Option|_|) (t : FSharpType) =
            match t with
                | Generic("Option", [a]) -> Some a
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

    let valueAdapter =
        {
            aType = Some "IMod<_>"
            aInit = sprintf "ResetMod(%s)"
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

            
            | Option (DomainType t) ->
                let tName = FSharpType.mutableName t
                {
                    aType = Some "IMod<_>"
                    aInit = fun fName -> sprintf "ResetMapOption(%s, %s.Create, fun (m,i) -> m.Update(i))" fName tName
                }
                       
            | DomainType t ->
                let mName = FSharpType.mutableName t
                {
                    aType = None
                    aInit = sprintf "%s.Create(%s)" mName
                }


            | t -> valueAdapter
             

    [<AutoOpen>]
    module Glasdas =
        type FSharpField with
            member x.CleanName = x.Name.ToLower()
                
                
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
                                let adapter = 
                                    if FSharpField.treatAsValue f then
                                        valueAdapter
                                    else
                                        generateAdapter f.FieldType

                                let inputName = sprintf "__initial.%s" f.DisplayName
                                let init = sprintf "let _%s = %s" f.DisplayName (adapter.aInit inputName)

                                let access =
                                    match adapter.aType with
                                        | Some t -> sprintf "_%s :> %s" f.DisplayName t
                                        | None -> sprintf "_%s" f.DisplayName

                                f.DisplayName, init, access
                        )
                    
                    do! line "[<StructuredFormatDisplay(\"{AsString}\")>]"
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
                        do! line ""
                        do! line "static member Create(initial) = %s(initial)" mutableName
                        do! line ""
                        do! line "override x.ToString() = __current.ToString()"
                        do! line "member private x.AsString = sprintf \"%%A\" __current"

                    }

                elif e.IsFSharpUnion then   
                    
                    let cases = e.UnionCases
                    
                    
                    do! line "[<AbstractClass; System.Runtime.CompilerServices.Extension; StructuredFormatDisplay(\"{AsString}\")>]"
                    let baseType = scope "type %s() =" mutableName
                    do! baseType {
                        do! line "abstract member TryUpdate : %s -> bool" immutableName
                        do! line "abstract member AsString : string"
                        do! line ""
                        


                        let createValue = scope "static member private CreateValue(__model : %s) = " immutableName
                        do! createValue {
                            do! line "match __model with" 
                            do! push

                            
                            for c in cases do
                                let fields = c.UnionCaseFields |> Seq.map (fun f -> f.CleanName) |> Seq.toList
                                let rhs = ("__model" :: fields) |> String.concat ", "

                                let lhs = 
                                    let lhs = fields |> String.concat ", "
                                    if lhs = "" then ""
                                    else lhs |> sprintf "(%s)"
                                
                                do! line "| %s%s -> M%s(%s) :> %s" c.Name lhs c.Name rhs mutableName

                            do! pop
                        }

                        do! line ""
                        do! line "static member Create(v : %s) =" immutableName
                        do! line "    ResetMod(%s.CreateValue v) :> IMod<_>" mutableName
                        do! line ""

                        do! line "[<System.Runtime.CompilerServices.Extension>]"
                        do! line "static member Update(m : IMod<%s>, v : %s) =" mutableName immutableName
                        do! push
                        do! line "let m = unbox<ResetMod<%s>> m" mutableName
                        do! line "if not (m.GetValue().TryUpdate v) then"
                        do! line "    m.Update(%s.CreateValue v)" mutableName
                        do! pop

                    }
                    do! line ""
                    do! line ""

                    for c in cases do
                        let fields = c.UnionCaseFields |> Seq.map (fun f -> f.CleanName, f) |> Seq.toList
                        let args = fields |> Seq.map (fun (name,f) -> name, FSharpType.immutableName f.FieldType) |> Seq.toList

                        let annotatedFields = 
                            fields |> List.map (fun (_,f) -> 
                                f, generateAdapter f.FieldType
                            )

                        let argDef = ("__initial : " + immutableName) :: (args |> List.map (fun (n,t) -> sprintf "%s : %s" n t)) |> String.concat ", "
                        let mName = "M" + c.Name
                        let caseType = scope "and private %s(%s) =" mName argDef
                        do! caseType {
                            do! line "inherit %s()" mutableName
                            do! line ""
                            do! line "let mutable __current = __initial"
                            for (f, adapter) in annotatedFields do
                                do! line "let _%s = %s" f.CleanName (adapter.aInit f.CleanName)
                                
                                
                            for (f, adapter) in annotatedFields do
                                match adapter.aType with
                                    | Some t -> do! line "member x.%s = _%s :> %s" f.CleanName f.CleanName t
                                    | None -> do! line "member x.%s = _%s" f.CleanName f.CleanName 
                                        
                            do! line ""
                            do! line "override x.ToString() = __current.ToString()"
                            do! line "override x.AsString = sprintf \"%%A\" __current"
                            do! line ""

                            let tryUpdate = scope "override x.TryUpdate(__model : %s) = " immutableName
                            do! tryUpdate {
                                do! line "if System.Object.ReferenceEquals(__current, __model) then"
                                do! line "    true"
                                do! line "else"
                                do! push
                                do! line "match __model with"
                                do! push

                                let args = args |> List.map fst |> String.concat ","
                                let args =
                                    if args = "" then ""
                                    else sprintf "(%s)" args

                                do! line "| %s%s -> " c.Name args
                                do! push
                                do! line "__current <- __model"
                                for (f,a) in annotatedFields do
                                    do! line "_%s.Update(%s)" f.CleanName f.CleanName
                                do! line "true"
                                do! pop

                                do! line "| _ -> false"


                                do! pop
                                do! pop
                            }


                        }

                    do! line "[<AutoOpen>]"
                    let patterns = scope "module %sPatterns =" mutableName
                    do! patterns {
                        let allNames = cases |> Seq.map (fun c -> "M" + c.Name) |> String.concat "|" 
                        do! line "let (|%s|) (m : %s) =" allNames mutableName
                        do! push
                        do! line "match m with"
                        
                        for c in cases do
                            let fields = c.UnionCaseFields |> Seq.map (fun f -> f.CleanName, f) |> Seq.toList
                            let args = fields |> Seq.map (fun (name,_) -> sprintf "v.%s" name) |> String.concat ","
                            if args = "" then
                                do! line "| :? M%s as v -> M%s" c.Name c.Name
                            else
                                do! line "| :? M%s as v -> M%s(%s)" c.Name c.Name args

                        do! line "| _ -> failwith \"impossible\""

                        do! pop
                        ()
                    }


                else
                    do! error "[Domain] bad domain type: %A" e

        }

    let runWithOptions (options : FSharpProjectOptions) =
        async {
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

    let runInternal (fsProjPath : string) (references : Set<string>) (files : list<string>) =
        async {
            let dir = Path.GetDirectoryName fsProjPath
            let outDir = Path.Combine(dir, "..", "..", "bin", "Debug")

            let fsCorePath = @"C:\Program Files (x86)\Reference Assemblies\Microsoft\FSharp\.NETFramework\v4.0\4.4.0.0\FSharp.Core.dll"

            let references = Set.add fsCorePath references

            let options = getProjectOptions fsProjPath (Set.toList references) files
            return! runWithOptions options
        }

    let run (fsProjPath : string) (references : Set<string>) (files : list<string>) =
        let dir = Path.GetDirectoryName fsProjPath
        let outDir = Path.Combine(dir, "..", "..", "bin", "Debug")
      
        let references = 
            references |> Set.map (fun r ->
                if Path.IsPathRooted r then 
                    r
                else 
                    let rFile = Path.Combine(outDir, r)
                    if File.Exists rFile then rFile
                    else 
                        let rFile = Path.Combine(dir, r)
                        if File.Exists rFile then rFile
                        else r
            )
            
        let files = 
            files |> List.map (fun r ->
                if Path.IsPathRooted r then 
                    r
                else 
                    Path.Combine(dir, r)
            )

        runInternal fsProjPath references files

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