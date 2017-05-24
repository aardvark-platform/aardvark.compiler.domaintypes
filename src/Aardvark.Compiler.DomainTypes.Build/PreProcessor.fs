namespace Aardvark.Compiler.DomainTypes

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

        let mutableNameRef (prefix : string) (e : FSharpType) =
            if e.HasTypeDefinition  then
                let def = e.TypeDefinition
                let ns, m =
                    match def.Namespace with
                        | Some ns ->
                            if ns.Length + 1 < def.AccessPath.Length then
                                let m = def.AccessPath.Substring(ns.Length + 1) 
                                ns, m
                            else
                                ns, ""
                        | _ ->
                            "", def.AccessPath


                let access = 
                    match ns, m with
                        | "", "" -> "Mutable"
                        | ns, "" -> ns + ".Mutable"
                        | "", m -> "Mutable." + m
                        | ns, m -> ns + ".Mutable." + m

                let path = 
                    if access.StartsWith prefix then
                        access.Substring(prefix.Length)
                    else
                        access


                if path = "" then "M" + def.DisplayName
                else path + ".M" + def.DisplayName
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

        let mutableNameDef (e : FSharpEntity) =
            "M" + e.DisplayName 


    module FSharpField =
        let treatAsValue (f : FSharpField) =
            f.PropertyAttributes |> Seq.exists (fun a -> a.AttributeType.FullName = "Aardvark.Base.Incremental.TreatAsValueAttribute")

        let nonIncremental (f : FSharpField) =
            f.PropertyAttributes |> Seq.exists (fun a -> a.AttributeType.FullName = "Aardvark.Base.Incremental.NonIncrementalAttribute")

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

        let (|GenericAccessPath|_|) (t : FSharpType) =
            if t.HasTypeDefinition then
                let d = t.TypeDefinition
                let name = 
                    match d.AccessPath with
                        | "global" -> d.DisplayName
                        | p -> p + "." + d.DisplayName

                let targs = t.GenericArguments |> Seq.toList
                
                Some(name, targs)
            else
                None

        let (|Generic|_|) (t : FSharpType) =
            if t.HasTypeDefinition then
                let d = t.TypeDefinition
                let name = d.DisplayName
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

        let (|HMap|_|) (t : FSharpType) =
            match t with
                | Generic("hmap", [a;b]) -> Some(a,b)
                | _ -> None
                
        let (|PList|_|) (t : FSharpType) =
            match t with
                | Generic("plist", [a]) -> Some a
                | _ -> None
                
        let (|Option|_|) (t : FSharpType) =
            match t with
                | Generic("Option", [a]) -> Some a
                | Generic("option", [a]) -> Some a
                | _ -> None

        let (|ThreadPool|_|) (t : FSharpType) =
            match t with
                | GenericAccessPath("Aardvark.Base.ThreadPool", [a]) -> Some a
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
            let! scope = CodeGen.currentScope

            match t with
                | PList (DomainType t) ->
                    let tName = FSharpType.mutableNameRef scope t
                    return {
                        aType = Some "alist<_>"
                        aInit = fun fName -> sprintf "ResetMapList(%s, (fun _ -> %s.Create), fun (m : %s,i) -> m.Update(i))"  fName tName tName
                    }

                | PList t ->
                    return {
                        aType = Some "alist<_>"
                        aInit = sprintf "ResetList(%s)"
                    }

                | HSet (DomainType t) ->
                    let tName = FSharpType.mutableNameRef scope t
                    let mutable getKey = "unbox"
                    match tryGetUniqueField t with
                        | Some field ->
                            getKey <- sprintf "(fun (v : %s) -> v.%s :> obj)" t.TypeDefinition.FullName field
                        | None ->
                            let range = FSharpType.range t
                            do! warn 4321 range "the domain type %s has no field marked with PrimaryKeyAttribute but is used inside a hset. peformance could suffer. please consider adding a primary key." (FSharpType.prettyName t)
                            
                    return {
                        aType = Some "aset<_>"
                        aInit = fun fName -> sprintf "ResetMapSet(%s, %s, %s.Create, fun (m : %s,i) -> m.Update(i))" getKey fName tName tName
                    }

                | HSet t ->
                    return {
                        aType = Some "aset<_>"
                        aInit = fun fName -> sprintf "ResetSet(%s)" fName
                    }

                | HMap(k, DomainType t) ->
                    let tName = FSharpType.mutableNameRef scope t
                    return {
                        aType = Some "amap<_,_>"
                        aInit = fun fName -> sprintf "ResetMapMap(%s, (fun k v -> %s.Create(v)), (fun (m : %s,i) -> m.Update(i)))" fName tName tName
                    }

                | HMap(k, t) ->
                    return {
                        aType = Some "amap<_,_>"
                        aInit = fun fName -> sprintf "ResetMap(%s)" fName
                    }

            
                | Option (DomainType t) ->
                    let tName = FSharpType.mutableNameRef scope t
                    return {
                        aType = Some "IMod<_>"
                        aInit = fun fName -> sprintf "ResetMapOption(%s, %s.Create, fun (m : %s,i) -> m.Update(i))" fName tName tName
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
                    let mName = FSharpType.mutableNameRef scope t
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
            let defName = FSharpEntity.mutableNameDef e
            let targs = e.GenericParameters
            if targs.Count > 0 then
                let range = e.DeclarationLocation
                do! error 5432 range "cannot compile generic domain type %s" e.DisplayName

            if e.IsFSharpRecord then
                let! annotatedFields =
                    e.FSharpFields
                        |> Seq.toList
                        |> List.chooseC (fun f ->
                            codegen {
                                if FSharpField.nonIncremental f then
                                    return None
                                else
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

                                    return Some (f.DisplayName, init, access)
                            }
                    )
                    
                match annotatedFields with
                    | [] -> 
                        ()
                    | _ ->
                        do! line "[<StructuredFormatDisplay(\"{AsString}\")>]"
                        let typeDef = scope "type %s private(__initial : %s) =" defName immutableName
                        do! typeDef {
                            do! line "let mutable __current = __initial"

                            for (_, init, _) in annotatedFields do
                                do! line "%s" init
                        
                            do! line ""

                            for (fname, _, access) in annotatedFields do
                                do! line "member x.%s = %s" fname access

                            for f in e.FSharpFields do
                                if FSharpField.nonIncremental f then
                                    do! line "member x.%s = __initial.%s" f.DisplayName f.DisplayName
                    
                            do! line ""

                            let apply = scope "member x.Update(__model : %s) =" immutableName
                            do! apply {
                                do! line "if not (Object.ReferenceEquals(__model, __current)) then"
                                do! push

                                do! line "__current <- __model"
                
                                for f in e.FSharpFields do
                                    if not (FSharpField.nonIncremental f) then
                                        let fName = f.DisplayName
                                        do! line "_%s.Update(__model.%s)" fName fName
                                do! pop
                            }
                            do! line ""
                            do! line "static member Create(initial) = %s(initial)" defName
                            do! line ""
                            do! line "override x.ToString() = __current.ToString()"
                            do! line "member private x.AsString = sprintf \"%%A\" __current"

                        }

                        do! line ""
                        do! line ""


                        do! line "[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]"
                        let mModule = scope "module %s =" defName
                        do! mModule {
                            for (name,_,_) in annotatedFields do
                                do! line "let inline %s (m : %s) = m.%s" name defName name
                        }

                        do! line ""
                        do! line ""

            elif e.IsFSharpUnion then   
                    
                let cases = e.UnionCases
                    
                    
                do! line "[<AbstractClass; System.Runtime.CompilerServices.Extension; StructuredFormatDisplay(\"{AsString}\")>]"
                let baseType = scope "type %s() =" defName
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
                                
                            do! line "| %s%s -> M%s(%s) :> %s" c.Name lhs c.Name rhs defName

                        do! pop
                    }

                    do! line ""
                    do! line "static member Create(v : %s) =" immutableName
                    do! line "    ResetMod(%s.CreateValue v) :> IMod<_>" defName
                    do! line ""

                    do! line "[<System.Runtime.CompilerServices.Extension>]"
                    do! line "static member Update(m : IMod<%s>, v : %s) =" defName immutableName
                    do! push
                    do! line "let m = unbox<ResetMod<%s>> m" defName
                    do! line "if not (m.GetValue().TryUpdate v) then"
                    do! line "    m.Update(%s.CreateValue v)" defName
                    do! pop

                }
                do! line ""

                let singleCase = cases.Count = 1

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
                        do! line "inherit %s()" defName
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

                            if not singleCase then
                                do! line "| _ -> false"


                            do! pop
                            do! pop
                        }


                    }
                    
                    do! line ""
                    
                do! line ""
                do! line "[<AutoOpen>]"
                let patterns = scope "module %sPatterns =" defName
                do! patterns {
                    let allNames = cases |> Seq.map (fun c -> "M" + c.Name) |> String.concat "|" 
                    do! line "let (|%s|) (m : %s) =" allNames defName
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
                    do! CodeGen.pushScope name
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
                    do! CodeGen.popScope
                    
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
                    do! CodeGen.pushScope ns
                    do! line "namespace %s" ns
                    do! line ""
                    do! line "open System"
                    do! line "open Aardvark.Base"
                    do! line "open Aardvark.Base.Incremental"
                    do! line "open %s" ns
                    do! line ""
                    do! line "[<AutoOpen>]"
                    do! line "module Mutable ="
                    do! line ""
                    do! push
                    do! CodeGen.pushScope "Mutable"
                    for c in children do
                        do! generateMutableModelInternal file c
                    do! pop
                    do! CodeGen.popScope

                | Namespace(None, [Module(name, false, false, children)])
                | Module(name, false, false, children) ->
                
                    do! CodeGen.pushScope name
                    do! line "module %s" name
                    do! line ""
                    do! line "open System"
                    do! line "open Aardvark.Base.Incremental"
                    do! line ""
                    for c in children do
                        do! generateMutableModelInternal file c
                    do! CodeGen.popScope

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
                                scope = []
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
