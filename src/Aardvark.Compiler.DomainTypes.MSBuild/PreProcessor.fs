﻿namespace Aardvark.Compiler.DomainTypes

open System
open System.IO
open Microsoft.Build.Framework
open Microsoft.Build.Utilities
open CodeGen
open Microsoft.FSharp.Compiler
open Microsoft.FSharp.Compiler.SourceCodeServices
    
module List =
    let rec mapOption (f : 'a -> Option<'b>) (l : list<'a>) =
        match l with
            | [] -> Some []
            | h :: t ->
                match f h, mapOption f t with
                    | Some h, Some t -> Some (h :: t)
                    | _ -> None 



type FileProcessingResult<'a> =
    | Faulted of warnings : list<ErrorInfo> * error : ErrorInfo
    | Finished of warnings : list<ErrorInfo> * 'a

type PreprocessingResult<'a> = 
    | CompilerError of list<FSharpErrorInfo>
    | Worked of Map<string, FileProcessingResult<'a>>
    


module Preprocessing =

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
        let range (t : FSharpType) =
            if t.HasTypeDefinition then
                t.TypeDefinition.DeclarationLocation
            else
                Range.range0

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
            
            if e.HasTypeDefinition then
                let def = e.TypeDefinition

                let qualifiedName = 
                    match def.AccessPath with
                        | "global" -> def.DisplayName
                        | ns -> ns + "." + def.DisplayName

                let targs = e.GenericArguments |> Seq.toList
                match targs with
                    | [] -> 
                        qualifiedName
                    | _ ->
                        let targs = targs |> List.map immutableName |> String.concat ", " |> sprintf "<%s>"
                        qualifiedName + targs
            else
                failwithf "[Domain] no definition for %A" e 

        let rec prettyName (e : FSharpType) =
            if e.HasTypeDefinition  then
                let def = e.TypeDefinition
                let targs = e.GenericArguments |> Seq.toList
                match targs with
                    | [] -> 
                        def.DisplayName
                    | _ ->
                        let targs = targs |> List.map immutableName |> String.concat ", " |> sprintf "<%s>"
                        def.DisplayName + targs
            else
                failwithf "[Domain] no definition for %A" e 

    module FSharpEntity =
        let immutableName (e : FSharpEntity) =
            match e.TryFullName with
                | Some f -> f
                | None -> e.DisplayName

        let mutableName (e : FSharpEntity) =
            "M" + e.DisplayName 

        let mutableNameSpace (e : FSharpEntity) =
            let name = e.FullName
            let idx = name.LastIndexOf '.'
            name.Substring(0, idx)

    module FSharpField =
        let treatAsValue (f : FSharpField) =
            f.PropertyAttributes |> Seq.exists (fun a -> a.AttributeType.FullName = "Aardvark.Base.Incremental.TreatAsValueAttribute")

    type EntityTree =
        | Namespace of Option<string> * list<EntityTree>
        | Module of name : string * autoOpen : bool * moduleSuffix : bool * content : list<EntityTree>
        | Type of FSharpEntity

    let rec buildEntityTree (e : FSharpImplementationFileDeclaration) : Option<EntityTree> =
        match e with
            | Entity(e, children) ->
                if e.IsNamespace then
                    let children = children |> List.choose buildEntityTree 
                    
                    match children with
                        | [] -> None
                        | [Namespace(Some c, children)] ->
                            Namespace(Some (e.DisplayName + "." + c), children) |> Some
                        | _ -> Namespace(Some e.DisplayName, children) |> Some

                elif e.IsFSharpModule then
                    let hasModuleSuffix = e.HasFSharpModuleSuffix
                    let hasAutoOpen = e.Attributes |> Seq.exists (fun a -> a.AttributeType.FullName = "Microsoft.FSharp.Core.AutoOpenAttribute")
                    let children = children |> List.choose buildEntityTree 
                    match children with
                        | [] -> None
                        | _ -> 
                            Module(e.DisplayName, hasAutoOpen, hasModuleSuffix, children) |> Some

                else
                    if e.Attributes |> Seq.exists FSharpAttribute.isDomainAttribute then
                        Type(e) |> Some
                    else
                        None
            | _ ->
                None

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
            
        let (|Tuple|_|) (t : FSharpType) =
            if t.IsTupleType then
                t.GenericArguments |> Seq.toList |> Some
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
             
    type FSharpField with
        member x.CleanName = x.Name.ToLower()
      

    let generateAdapter (t : FSharpType) =
        codegen {
            match t with
                | PList (DomainType t) ->
                    let tName = FSharpType.mutableName t
                    return {
                        aType = Some "alist<_>"
                        aInit = fun fName -> sprintf "ResetMapList(%s, (fun _ -> %s.Create), fun (m,i) -> m.Update(i))"  fName tName
                    }

                | PList t ->
                    return {
                        aType = Some "alist<_>"
                        aInit = sprintf "ResetList(%s)"
                    }

                | HSet (DomainType t) ->
                    let tName = FSharpType.mutableName t
                    let mutable getKey = "unbox"
                    match tryGetUniqueField t with
                        | Some field ->
                            getKey <- sprintf "(fun v -> v.%s :> obj)" field
                        | None ->
                            let range = FSharpType.range t
                            do! warn 4321 range "the domain type %s has no field marked with PrimaryKeyAttribute but is used inside a hset. peformance could suffer. please consider adding a primary key." (FSharpType.prettyName t)
                            
                    return {
                        aType = Some "aset<_>"
                        aInit = fun fName -> sprintf "ResetMapSet(%s, %s, %s.Create, fun (m,i) -> m.Update(i))" getKey fName tName
                    }

                | HSet t ->
                    let tName = FSharpType.mutableName t
                    return {
                        aType = Some "aset<_>"
                        aInit = fun fName -> sprintf "ResetSet(%s)" fName
                    }

            
                | Option (DomainType t) ->
                    let tName = FSharpType.mutableName t
                    return {
                        aType = Some "IMod<_>"
                        aInit = fun fName -> sprintf "ResetMapOption(%s, %s.Create, fun (m,i) -> m.Update(i))" fName tName
                    }
                       
                | Tuple types ->
                    let allDomain = types |> List.forall FSharpType.isDomainType
                    let noDomain = types |> List.forall (not << FSharpType.isDomainType)

                    if allDomain then
                        return! error 5432 Range.range0 "domain tuples not implemented"

                    elif noDomain then
                        return valueAdapter

                    else
                        return! error 5432 Range.range0  "mixed tuples not implemented"
                    

                | DomainType t ->
                    let mName = FSharpType.mutableName t
                    return {
                        aType = None
                        aInit = sprintf "%s.Create(%s)" mName
                    }


                | t -> 
                    return valueAdapter
             
        }

    let generateMutableType (e : FSharpEntity) =
        codegen {
            let immutableName = FSharpEntity.immutableName e
            let mutableName = FSharpEntity.mutableName e
            let targs = e.GenericParameters
            if targs.Count > 0 then
                let range = e.DeclarationLocation
                do! error 5432 range "cannot compile generic domain type %s" e.DisplayName

            if e.IsFSharpRecord then
                let! annotatedFields =
                    e.FSharpFields
                        |> Seq.toList
                        |> List.mapC (fun f ->
                            codegen {
                                let! adapter = 
                                    if FSharpField.treatAsValue f then
                                        codegen { return valueAdapter }
                                    else
                                        generateAdapter f.FieldType

                                let inputName = sprintf "__initial.%s" f.DisplayName
                                let init = sprintf "let _%s = %s" f.DisplayName (adapter.aInit inputName)

                                let access =
                                    match adapter.aType with
                                        | Some t -> sprintf "_%s :> %s" f.DisplayName t
                                        | None -> sprintf "_%s" f.DisplayName

                                return f.DisplayName, init, access
                            }
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

                do! line ""
                do! line ""


                do! line "[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]"
                let mModule = scope "module %s =" mutableName
                do! mModule {
                    for (name,_,_) in annotatedFields do
                        do! line "let inline %s (m : %s) = m.%s" name mutableName name
                }

                do! line ""
                do! line ""

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

                for c in cases do
                    let fields = c.UnionCaseFields |> Seq.map (fun f -> f.CleanName, f) |> Seq.toList
                    let args = fields |> Seq.map (fun (name,f) -> name, FSharpType.immutableName f.FieldType) |> Seq.toList

                    let! annotatedFields = 
                        fields |> List.mapC (fun (_,f) -> 
                            codegen {
                                let! a = generateAdapter f.FieldType
                                return f, a
                            }
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
                    
                    do! line ""
                    
                do! line ""
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
                }

                
                do! line ""
                do! line ""


            else
                do! error 5432 e.DeclarationLocation "[Domain] bad domain type: %A" e

        }
    
    let generateLenses (e : FSharpEntity) =
        codegen {
            let eType = FSharpEntity.immutableName e
            let name = e.DisplayName
            if e.IsFSharpRecord then

                do! line "[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]"
                let mModule = scope "module %s =" name
                do! mModule {
                    do! line "[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]"
                    let lensModule = scope "module Lens ="
                    do! lensModule {
                        for f in e.FSharpFields do
                            let fName = f.Name
                            let fType = FSharpType.immutableName f.FieldType

                            let lens = scope "let %s =" fName
                            do! lens {
                                do! line "{ new Lens<%s, %s>() with" eType fType
                                do! push
                                
                                do! line "override x.Get(r) = r.%s" fName
                                do! line "override x.Set(r,v) = { r with %s = v }" fName
                                do! line "override x.Update(r,f) = { r with %s = f r.%s }" fName fName

                                do! pop
                                do! line "}"
                            }
                    }
                }

        }



    let rec generateMutableModelInternal (file : string) (l : EntityTree) =
        codegen {
            match l with
                | Namespace(ns, children) ->
                    match ns with
                        | Some ns -> 
                            let range = Range.mkRange file (Range.mkPos 0 0) (Range.mkPos 0 0)
                            do! error 5432 range "found nested namespace %A. namespaces can only occur at top-level" ns
                        | None ->
                            ()

                    for c in children do
                        do! generateMutableModelInternal file c

                | Module(name, autoOpen, suffix, children) ->
                    if autoOpen then
                        do! line "[<AutoOpen>]"
                    if suffix then
                        do! line "[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]"
                    do! line "module %s =" name
                    do! push
                    do! line "open %s" name
                    do! line ""
                    for c in children do
                        do! generateMutableModelInternal file c
                    do! pop
                    
                | Type(e) ->
                    do! generateMutableType e
                    do! line ""
                    do! line ""
                    do! generateLenses e
        }

    let generateMutableModel (file : string) (l : EntityTree) =
        codegen {
            match l with
                | Namespace(Some ns, children) ->
                    do! line "namespace %s" ns
                    do! line ""
                    do! line "open System"
                    do! line "open Aardvark.Base"
                    do! line "open Aardvark.Base.Incremental"
                    do! line ""
                    do! line "[<AutoOpen>]"
                    do! line "module Mutable ="
                    do! line ""
                    do! push
                    for c in children do
                        do! generateMutableModelInternal file c
                    do! pop

                | Namespace(None, [Module(name, false, false, children)])
                | Module(name, false, false, children) ->
                    do! line "module %s" name
                    do! line ""
                    do! line "open System"
                    do! line "open Aardvark.Base.Incremental"
                    do! line ""
                    for c in children do
                        do! generateMutableModelInternal file c

                | _ ->
                    let range = Range.mkRange file (Range.mkPos 0 0) (Range.mkPos 0 0)
                    do! error 5432 range "invalid layout: does not start with a module or namespace"

                    
        }

    let generateMutableModels (file : string) (l : list<EntityTree>) =
        codegen {
            for e in l do
               do! generateMutableModel file e
        }

    let runWithOptions (options : FSharpProjectOptions) =
        async {
            let! res = checker.ParseAndCheckProject(options)

            if res.HasCriticalErrors then
                return CompilerError (Array.toList res.Errors)

            else
                let entityTrees = 
                    res.AssemblyContents.ImplementationFiles |> List.choose (fun f ->
                        let domainTypes = f.Declarations |> Seq.choose buildEntityTree |> Seq.toList
                        match domainTypes with
                            | [] -> None
                            | _ -> Some (f.FileName, domainTypes)
                    )
                    |> Map.ofList

                let code = 
                    entityTrees |> Map.map (fun f c -> 
                        let c = generateMutableModels f c
                        let state, info = 
                            c.generate {
                                result = System.Text.StringBuilder()
                                indent = ""
                                warnings = []
                            }
                        match info with
                            | Success () ->
                                Finished(state.warnings, state.result.ToString())
                            | Error e ->
                                Faulted(state.warnings, e)
                    )
                return Worked code
        }

    let runInternal (fsProjPath : string) (references : Set<string>) (files : list<string>) =
        async {
            let dir = Path.GetDirectoryName fsProjPath
            let outDir = Path.Combine(dir, "..", "..", "bin", "Debug")

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

[<AutoOpen>]
module Extensions =
    type TaskLoggingHelper with
        member x.LogWarning(w : ErrorInfo) =
            x.LogWarning(
                "", 
                string w.code, 
                "", 
                w.file, 
                w.startLine, w.startColumn + 1, 
                w.endLine, w.endColumn + 1, 
                w.message,
                [||]
            )

        member x.LogError(w : ErrorInfo) =
            x.LogError(
                "", 
                string w.code, 
                "", 
                w.file, 
                w.startLine, w.startColumn + 1, 
                w.endLine, w.endColumn + 1, 
                w.message,
                [||]
            )

type Preprocess() =
    inherit Task()

    let mutable debug = false
    let mutable files : string[] = [||]
    let mutable references : string[] = [||]
    let mutable projectFile = ""
    let mutable results : string[] = [||]

    override x.Execute() =
        if debug then
            System.Diagnostics.Debugger.Launch() |> ignore

        let prep = Preprocessing.run projectFile (Set.ofArray references) (Array.toList files) |> Async.RunSynchronously
        results <- files

        let projectDir = Path.GetDirectoryName projectFile

        match prep with
            | Worked res ->
                let mutable goodFiles = Set.empty
                for (f,content) in Map.toSeq res do
                    match content with
                        | Finished(warnings, content) ->
                            for w in warnings do 
                                x.Log.LogWarning(w)

                            goodFiles <- Set.add f goodFiles

                            let path = System.IO.Path.ChangeExtension(f, ".g.fs")
                            x.Log.LogMessage(sprintf "generated DomainFile %s" (Path.GetFileName path))

                            let old = 
                                if File.Exists path then File.ReadAllText path
                                else ""

                            if old <> content then
                                File.WriteAllText(path, content)
                        | Faulted(warnings, error) ->
                            for w in warnings do x.Log.LogWarning(w)
                            x.Log.LogError(error)

                let files = 
                    files |> Array.map (fun f ->
                        Path.Combine(projectDir, f) |> Path.GetFullPath
                    )

                let fscFiles =
                    files |> Array.collect (fun f ->
                        let gf = System.IO.Path.ChangeExtension(f, ".g.fs")

                        if files |> Array.exists (fun f -> f = gf) then
                            [| f |]
                        else
                            if Set.contains f goodFiles then
                                [| f; gf |]
                            else
                                [| f |]
                    )

                results <- fscFiles

                true

            | CompilerError errors ->
                x.Log.LogError("F# compiler returned errors")
                false


    member x.Debug
        with get() = debug
        and set i = debug <- i

    [<Required>]
    member x.Files
        with get() = files
        and set i = files <- i

    [<Required>]
    member x.References
        with get() = references
        and set c = references <- c

    [<Required>]
    member x.ProjectFile
        with get() = projectFile
        and set f = projectFile <- f

   
    [<Output>]
    member x.Results
        with get() = results
        and set r = results <- r