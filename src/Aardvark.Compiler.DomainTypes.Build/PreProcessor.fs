namespace Aardvark.Compiler.DomainTypes

open System
open System.IO
open Microsoft.Build.Framework
open Microsoft.Build.Utilities
open CodeGen
open Microsoft.FSharp.Compiler
open Microsoft.FSharp.Compiler.SourceCodeServices 
open System.Collections.Concurrent
        
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



module TypeTree =

    [<AutoOpen>]
    module private Utilities =
        let private domainAttName = "Aardvark.Base.Incremental.DomainTypeAttribute"
        let private primAttName = "Aardvark.Base.Incremental.PrimaryKeyAttribute"
        let private nonIncrementalAttName = "Aardvark.Base.Incremental.NonIncrementalAttribute"
        let private treatAsValueAttName = "Aardvark.Base.Incremental.TreatAsValueAttribute"


        module FSharpAttribute =
            let isDomainAttribute (a : FSharpAttribute) =
                a.AttributeType.FullName = domainAttName
                
            let isPrimaryKey (a : FSharpAttribute) =
                a.AttributeType.FullName = primAttName

            let isNonIncremental (a : FSharpAttribute) = 
                a.AttributeType.FullName = nonIncrementalAttName

            let isTreatAsValue (a : FSharpAttribute) = 
                a.AttributeType.FullName = treatAsValueAttName

        module FSharpEntity = 
            let isDomainType (t : FSharpEntity) =
                t.Attributes |> Seq.exists FSharpAttribute.isDomainAttribute
                
            let tryGetPrimaryKeyField (t : FSharpEntity) =
                let res = 
                    if t.IsFSharpRecord then
                        t.FSharpFields |> Seq.tryPick (fun f ->
                            if f.PropertyAttributes |> Seq.exists FSharpAttribute.isPrimaryKey then
                                Some f.DisplayName
                            else
                                None
                        )
                    else
                        None

                match res with
                    | Some v -> Some v
                    | None ->
                        t.MembersFunctionsAndValues |> Seq.tryPick (fun m ->
                            if m.Attributes |> Seq.exists FSharpAttribute.isPrimaryKey then
                                Some m.DisplayName
                            else
                                None
                        )

            let range (e : FSharpEntity) = e.DeclarationLocation

        module FSharpField =
            let isNonIncremental (f : FSharpField) =
                f.PropertyAttributes |> Seq.exists FSharpAttribute.isNonIncremental

            let treatAsValue (f : FSharpField) =
                f.PropertyAttributes |> Seq.exists FSharpAttribute.isTreatAsValue

    type DomainTypeKind =
        | Adaptive
        | Persistent
        | Resettable
        | Unknown

    [<CustomEquality; NoComparison>]
    type TypeDef = 
        { 
            kind : DomainTypeKind
            scope : string
            path : list<string>
            name : string
            tpars : list<string>
            primaryKey : Option<string> 
            range : Range.range
            mutable fields : list<Field>
        } with
        
        override x.GetHashCode() = (x.fullName "").GetHashCode()
        override x.Equals(o) = 
            match o with
                | :? TypeDef as o -> x.fullName "" = o.fullName ""
                | _ -> false

        member x.IsGeneric = x.tpars.IsEmpty |> not

        member x.relativeName (ref : TypeDef) =
            let other = 
                match ref.scope with
                    | "" -> ref.path 
                    | s -> ((s.Split('.') |> Array.toList) @ ref.path)

            x.fullName (String.concat "." other)


        member x.fullName (scope : string) =
            let s = scope.Split([|'.'|], StringSplitOptions.RemoveEmptyEntries) |> Array.toList 
            let all = 
                match x.scope with
                    | "" -> x.path @ [x.name]
                    | s -> ((s.Split('.') |> Array.toList) @ x.path) @ [x.name]

            let rec strip (p : list<string>) (n : list<string>) =
                match p, n with
                    | ph :: pt, nh :: nt ->
                        if ph = nh then strip pt nt
                        else all
                    | [], n -> n

                    | _ -> all

            
            String.concat "." (strip s all)

    and TypeRef = 
        | GenericParameter of name : string
        | Reference of def : TypeDef * targs : array<TypeRef>
        | Tuple of list<TypeRef>
        | Array of TypeRef with

            member x.genericParameters =
                match x with
                    | Reference(def, targs) -> targs |> Seq.map (fun t -> t.genericParameters) |> Set.unionMany
                    | GenericParameter p -> Set.singleton p
                    | Tuple ts -> ts |> List.map (fun t -> t.genericParameters) |> Set.unionMany
                    | Array t -> t.genericParameters

            member x.fullName (scope : string) = 
                match x with
                    | GenericParameter n ->
                        sprintf "'%s" n

                    | Reference(def,targs) ->
                        let dn = def.fullName scope
                        if targs.Length > 0 then
                            let targs = targs |> Seq.map (fun t -> t.fullName scope) |> String.concat ","
                            sprintf "%s<%s>" dn targs
                        else
                            dn
                    | Tuple types ->
                        let types = types |> Seq.map (fun t -> t.fullName scope) |> String.concat " * "
                        sprintf "(%s)" types

                    | Array t ->
                        let n = t.fullName scope
                        sprintf "%s[]" n

            member x.name (scope : string) =
                match x with
                    | GenericParameter n ->
                        sprintf "'%s" n

                    | Reference(def,targs) ->
                        let dn = def.name
                        if targs.Length > 0 then
                            let targs = targs |> Seq.map (fun t -> t.fullName scope) |> String.concat ","
                            sprintf "%s<%s>" dn targs
                        else
                            dn
                    | Tuple types ->
                        let types = types |> Seq.map (fun t -> t.fullName scope) |> String.concat " * "
                        sprintf "(%s)" types

                    | Array t ->
                        let n = t.fullName scope
                        sprintf "%s[]" n
            
            member x.ContainsGenericParameter =
                match x with
                    | Array t -> t.ContainsGenericParameter
                    | Tuple ts -> ts |> List.exists (fun t -> t.ContainsGenericParameter)
                    | Reference(_,ts) -> ts |> Array.exists (fun t -> t.ContainsGenericParameter)
                    | GenericParameter _ -> true
 
    and Field =
        {
            name            : string
            fieldType       : TypeRef
            description     : DomainTypeDescription
            nonIncremental  : bool
            treatAsValue    : bool
        }

    and UnionCase =
        {
            caseName : string
            fields : list<Field>
        }

    and DomainTypeDescription =
        {
            aSimple     : bool
            aType       : TypeRef
            aInit       : string -> string -> string
            aUpdate     : string -> string -> string -> string
            aView       : string -> string -> string
        }


    module private Injected =
        let mutable internal injectedOfTypeRef : (TypeRef -> unit) -> Map<string, bool> -> bool -> TypeRef -> DomainTypeDescription =
            Unchecked.defaultof<_>

        let mutable internal injectedOfFSharpField : (TypeRef -> unit) -> Map<string, bool> -> FSharpField -> Field =
            Unchecked.defaultof<_>
            
        

    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module TypeDef =
        let inline kind (t : TypeDef) = t.kind
        let inline scope (t : TypeDef) = t.scope
        let inline path (t : TypeDef) = t.path
        let inline name (t : TypeDef) = t.name
        let inline tpars (t : TypeDef) = t.tpars
        let inline primaryKey (t : TypeDef) = t.primaryKey
        let inline fields (t : TypeDef) = t.fields
        let inline isGeneric (t : TypeDef) = t.IsGeneric
        let inline fullName (scope : string) (t : TypeDef) = t.fullName scope

        let int = { range = Range.range0; fields = []; kind = Unknown; scope = "Microsoft.FSharp.Core"; path = []; name = "int"; tpars = []; primaryKey = None }

        let imod = { range = Range.range0; fields = [];  kind = Adaptive; scope = "Aardvark.Base.Incremental"; path = []; name = "IMod"; tpars = ["a"]; primaryKey = None }
        let resetmod = { range = Range.range0; fields = []; kind = Resettable; scope = "Aardvark.Base.Incremental"; path = []; name = "ResetMod"; tpars = ["a"]; primaryKey = None }

        let hset = { range = Range.range0; fields = []; kind = Persistent; scope = "Aardvark.Base"; path = []; name = "hset"; tpars = ["a"]; primaryKey = None }
        let aset = { range = Range.range0; fields = []; kind = Adaptive; scope = "Aardvark.Base.Incremental"; path = []; name = "aset"; tpars = ["a"]; primaryKey = None }
        let resetset = { range = Range.range0; fields = []; kind = Resettable; scope = "Aardvark.Base.Incremental"; path = []; name = "ResetSet"; tpars = ["a"]; primaryKey = None }
 
        let plist = { range = Range.range0; fields = []; kind = Persistent; scope = "Aardvark.Base"; path = []; name = "plist"; tpars = ["a"]; primaryKey = None }
        let alist = { range = Range.range0; fields = []; kind = Adaptive; scope = "Aardvark.Base.Incremental"; path = []; name = "alist"; tpars = ["a"]; primaryKey = None }
        let resetlist = { range = Range.range0; fields = []; kind = Resettable; scope = "Aardvark.Base.Incremental"; path = []; name = "ResetList"; tpars = ["a"]; primaryKey = None }

        let hmap = { range = Range.range0; fields = []; kind = Persistent; scope = "Aardvark.Base"; path = []; name = "hmap"; tpars = ["k"; "v"]; primaryKey = None}
        let amap = { range = Range.range0; fields = []; kind = Adaptive; scope = "Aardvark.Base.Incremental"; path = []; name = "amap"; tpars = ["k"; "v"]; primaryKey = None }
        let resetmap = { range = Range.range0; fields = []; kind = Resettable; scope = "Aardvark.Base.Incremental"; path = []; name = "ResetMap"; tpars = ["k"; "v"]; primaryKey = None }

        let option = { range = Range.range0; fields = []; kind = Persistent; scope = "Microsoft.FSharp.Core"; path = []; name = "option"; tpars = ["a"]; primaryKey = None}
        let Option = { range = Range.range0; fields = []; kind = Persistent; scope = "Microsoft.FSharp.Core"; path = []; name = "Option"; tpars = ["a"]; primaryKey = None}

        let instantiate (targs : list<TypeRef>) (d : TypeDef) =
            let targs = List.toArray targs
            if d.tpars.Length <> targs.Length then
                failwithf "[TypeDef] cannot instantiate %s with %A (expecing %d type arguments)" (d.fullName "") targs d.tpars.Length
                 
            Reference (d, targs)


        let private cache = ConcurrentDictionary<Map<string, bool> * FSharpEntity, TypeDef>()

        let ofFSharpDefinition (needPrimaryKey : TypeRef -> unit) (simple : Map<string, bool>) (d : FSharpEntity) =
            let mutable isNew = false
            let result = 
                cache.GetOrAdd((simple, d), fun (simple, d) ->
                    isNew <- true
                    let path = 
                        match d.AccessPath with
                            | "global" -> []
                            | p -> p.Split('.') |> Array.toList

                    let scope = 
                        match d.Namespace with
                            | Some ns -> ns.Split('.') |> Array.toList
                            | None -> []

                    let rec skipScope (s : list<string>) (p : list<string>) =
                        match s, p with
                            | [], p -> p
                            | sh :: st, ph :: pt ->
                                if sh = ph then skipScope st pt
                                else failwith "asdsadsad"
                            | _ ->
                                failwith "asdsadsad"
                    let path = skipScope scope path

                    let kind =
                        if FSharpEntity.isDomainType d then 
                            Persistent
                        else
                            let full = d.AccessPath + d.DisplayName
                            match full with
                                | "Aardvark.Base.Incremental.IMod" -> Adaptive
                                | "Aardvark.Base.Incremental.aset" -> Adaptive
                                | "Aardvark.Base.Incremental.alist" -> Adaptive
                                | "Aardvark.Base.Incremental.amap" -> Adaptive
                                | "Aardvark.Base.Incremental.MSet" -> Resettable
                                | "Aardvark.Base.Incremental.MList" -> Resettable
                                | "Aardvark.Base.Incremental.MMap" -> Resettable
                                | "Aardvark.Base.Incremental.MOption" -> Resettable
                                | _ -> 
                                    Unknown
                                    
                    { 
                        kind = kind
                        scope = String.concat "." scope
                        path = path
                        name = d.DisplayName
                        tpars = d.GenericParameters |> Seq.toList |> List.map (fun p -> p.Name)
                        primaryKey = FSharpEntity.tryGetPrimaryKeyField d
                        fields = []
                        range = FSharpEntity.range d
                    }
                )

            if isNew && result.kind = Persistent then
                result.fields <- d.FSharpFields |> Seq.toList |> List.map (Injected.injectedOfFSharpField needPrimaryKey simple)
            result

    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module TypeRef =

        let makeTuple (elems : list<TypeRef>) =
            Tuple elems

        let makeArray (elemType : TypeRef) =
            Array elemType
        
        let rec substitute (m : string -> Option<TypeRef>) (t : TypeRef) =
            match t with
                | GenericParameter n ->
                    match m n with
                        | Some t -> t
                        | _ -> t
                | Reference(def, targs) ->
                    Reference(def, targs |> Array.map (substitute m))

                | Tuple ts ->
                    Tuple (ts |> List.map (substitute m))
                | Array t ->
                    Array (substitute m t)
        
        let rec ofFSharpType (needPrimaryKey : TypeRef -> unit) (simple : Map<string, bool>) (t : FSharpType) =
            if t.IsGenericParameter then
                GenericParameter t.GenericParameter.Name
            else
                let targs = t.GenericArguments |> Seq.map (ofFSharpType needPrimaryKey simple)

                if t.IsTupleType then
                    Tuple (Seq.toList targs)

                elif t.HasTypeDefinition then
                    let def = t.TypeDefinition

                    if def.IsArrayType then
                        let et = ofFSharpType needPrimaryKey simple t.GenericArguments.[0]
                        Array et
                    else
                        let typeDef = TypeDef.ofFSharpDefinition needPrimaryKey simple t.TypeDefinition
                        Reference(typeDef, Seq.toArray targs)

                else
                    failwith ""

    [<AutoOpen>]
    module TypeRefPatterns = 
        let (|HSet|_|) (t : TypeRef) =
            match t with
                | Reference(def, [| et |]) when def.fullName "" = TypeDef.hset.fullName "" -> Some et
                | _ -> None
            
        let (|PList|_|) (t : TypeRef) =
            match t with
                | Reference(def, [| et |]) when def.fullName "" = TypeDef.plist.fullName "" -> Some et
                | _ -> None
            
        let (|HMap|_|) (t : TypeRef) =
            match t with
                | Reference(def, [| k; v |]) when def.fullName "" = TypeDef.hmap.fullName "" -> Some(k,v)
                | _ -> None
      
        let (|Option|_|) (t : TypeRef) =
            match t with
                | Reference(def, [| v |]) when def.fullName "" = TypeDef.Option.fullName "" || def.fullName "" = TypeDef.option.fullName "" -> Some(v)
                | _ -> None



    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Field =

        let ofFSharpField (needPrimaryKey : TypeRef -> unit) (simple : Map<string, bool>) (f : FSharpField) =
            let t = TypeRef.ofFSharpType needPrimaryKey simple f.FieldType
            {
                name            = f.DisplayName
                fieldType       = t
                description     = Injected.injectedOfTypeRef needPrimaryKey simple false t
                nonIncremental  = FSharpField.isNonIncremental f
                treatAsValue    = FSharpField.treatAsValue f
            }

        do Injected.injectedOfFSharpField <- ofFSharpField



    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module UnionCase =
        let ofFSharpUnionCase (needPrimaryKey : TypeRef -> unit) (simple : Map<string, bool>) (c : FSharpUnionCase) =
            {
                caseName = c.DisplayName
                fields = c.UnionCaseFields |> Seq.toList |> List.map (Field.ofFSharpField needPrimaryKey simple)
            }
        


    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module DomainTypeDescription =
            
        let rec ofTypeRef (needPrimaryKey : TypeRef -> unit) (simple : Map<string, bool>) (isByRef : bool) (rt : TypeRef) : DomainTypeDescription =
            match rt with
                
                | Array _ ->
                    failwith "not implemented"

                | GenericParameter n ->
                    let isSimple = 
                        match Map.tryFind n simple with
                            | Some v -> v
                            | None -> false

                    if isSimple then
                        {
                            aSimple = true
                            aType = TypeDef.imod |> TypeDef.instantiate [rt]
                            aInit = fun _ -> sprintf "ResetMod.Create(%s)"
                            aUpdate = fun _ -> sprintf "ResetMod.Update(%s,%s)"
                            aView = fun _ -> sprintf "%s :> IMod<_>"
                        }

                    else
                        let name =
                            if isByRef then "n" + n
                            else "v" + n
                        {
                            aSimple = false
                            aType = GenericParameter name
                            aInit = fun _ -> sprintf "__%sinit(%s)" n
                            aUpdate = fun _ -> sprintf "__%supdate(%s, %s)" n
                            aView = fun _ -> sprintf "__%sview(%s)" n
                        }
                    
                | Option t ->
                    let desc = ofTypeRef needPrimaryKey simple true t
                    if desc.aSimple then
                        {
                            aSimple = false
                            aType = TypeDef.imod |> TypeDef.instantiate [rt]
                            aInit = fun _ -> sprintf "MOption.Create(%s)"
                            aUpdate = fun _ -> sprintf "MOption.Update(%s, %s)"
                            aView = fun _ -> sprintf "%s :> IMod<_>"
                        }
                    else
                        {
                            aSimple = false
                            aType = TypeDef.imod |> TypeDef.instantiate [TypeDef.option |> TypeDef.instantiate [desc.aType]]
                            aInit = fun s v -> sprintf "MOption.Create(%s, (fun v -> %s), (fun (m,v) -> %s), (fun v -> %s))" v (desc.aInit s "v") (desc.aUpdate s "m" "v") (desc.aView s "v")
                            aUpdate = fun _ -> sprintf "MOption.Update(%s, %s)"
                            aView = fun _ -> sprintf "%s :> IMod<_>"
                        }
                | HSet t ->
                    let desc = ofTypeRef needPrimaryKey simple true t
                    if desc.aSimple then
                        {
                            aSimple = false
                            aType = TypeDef.aset |> TypeDef.instantiate [t]
                            aInit = fun _ -> sprintf "MSet.Create(%s)"
                            aUpdate = fun _ -> sprintf "MSet.Update(%s, %s)"
                            aView = fun _ -> sprintf "%s :> aset<_>"
                        }
                    else
                        let key =
                            match t with
                                | Reference(def,_) -> def.primaryKey
                                | _ -> None

                        
                        let getKey s =
                            match key with
                                | Some k ->
                                    sprintf "(fun (v : %s) -> v.%s :> obj)" (t.fullName s) k
                                | None ->
                                    needPrimaryKey t
                                    "unbox"

                        {
                            aSimple = false
                            aType = TypeDef.aset |> TypeDef.instantiate [desc.aType]
                            aInit = fun s v -> sprintf "MSet.Create(%s, %s, (fun v -> %s), (fun (m,v) -> %s), (fun v -> %s))" (getKey s) v (desc.aInit s "v") (desc.aUpdate s "m" "v") (desc.aView s "v")
                            aUpdate = fun _ -> sprintf "MSet.Update(%s, %s)"
                            aView = fun _ -> sprintf "%s :> aset<_>"
                        }
                  
                | PList t ->
                    let desc = ofTypeRef needPrimaryKey simple true t
                    if desc.aSimple then
                        {
                            aSimple = false
                            aType = TypeDef.alist |> TypeDef.instantiate [t]
                            aInit = fun _ -> sprintf "MList.Create(%s)"
                            aUpdate = fun _ -> sprintf "MList.Update(%s, %s)"
                            aView = fun _ -> sprintf "%s :> alist<_>"
                        }
                    else
                        {
                            aSimple = false
                            aType = TypeDef.alist |> TypeDef.instantiate [desc.aType]
                            aInit = fun s v -> sprintf "MList.Create(%s, (fun v -> %s), (fun (m,v) -> %s), (fun v -> %s))" v (desc.aInit s "v") (desc.aUpdate s "m" "v") (desc.aView s "v")
                            aUpdate = fun _ -> sprintf "MList.Update(%s, %s)"
                            aView = fun _ -> sprintf "%s :> alist<_>"
                        }
                
                | HMap(k,v) ->
                    let desc = ofTypeRef needPrimaryKey simple true v
                    if desc.aSimple then
                        {
                            aSimple = false
                            aType = TypeDef.amap |> TypeDef.instantiate [k; v]
                            aInit = fun _ -> sprintf "MMap.Create(%s)"
                            aUpdate = fun _ -> sprintf "MMap.Update(%s, %s)"
                            aView = fun _ -> sprintf "%s :> amap<_,_>"
                        }
                    else
                        {
                            aSimple = false
                            aType = TypeDef.amap |> TypeDef.instantiate [k; desc.aType]
                            aInit = fun s v -> sprintf "MMap.Create(%s, (fun v -> %s), (fun (m,v) -> %s), (fun v -> %s))" v (desc.aInit s "v") (desc.aUpdate s "m" "v") (desc.aView s "v")
                            aUpdate = fun _ -> sprintf "MMap.Update(%s, %s)"
                            aView = fun _ -> sprintf "%s :> amap<_,_>"
                        }
                    
                | Tuple elems ->
                    let elemDesc = elems |> List.map (ofTypeRef needPrimaryKey simple false)
                    {
                        aSimple = false
                        
                        aType = Tuple (elemDesc |> List.map (fun e -> e.aType))
                        aInit = fun s v ->
                            let names = elems |> List.mapi (fun i _ -> sprintf "%s_item%d" v i)
                            let inits = List.map2 (fun i n -> i.aInit s n) elemDesc names
                            sprintf "let (%s) = %s in (%s)" (String.concat "," names) v (String.concat "," inits)

                        aUpdate = fun s m v -> 
                            let mnames = elems |> List.mapi (fun i _ -> sprintf "%s_item%d" m i)
                            let vnames = elems |> List.mapi (fun i _ -> sprintf "%s_item%d" v i)

                            String.concat "\r\n" [
                                yield sprintf "let (%s) = %s" (String.concat "," vnames) v
                                yield sprintf "let (%s) = %s" (String.concat "," mnames) m

                                for (mn, vn, d) in List.zip3 mnames vnames elemDesc do
                                    yield d.aUpdate s mn vn
                            ]

                        aView = fun s v ->
                            let names = elems |> List.mapi (fun i _ -> sprintf "%s_item%d" v i)
                            let views = List.map2 (fun i n -> i.aView s n) elemDesc names
                            sprintf "let (%s) = %s in (%s)" (String.concat "," names) v (String.concat "," views)
                        
                    }

                | Reference(def, targs) ->
                    match def.kind with
                        | Persistent ->
                            let resettable = 
                                {
                                    def with
                                        path = "Mutable" :: def.path
                                        name = "M" + def.name
                                }
                                        
                                        
                            let targDesc = targs |> Array.toList |> List.map (ofTypeRef needPrimaryKey simple false)

                            let aDef = 
                                {
                                    def with
                                        path = "Mutable" :: def.path
                                        name = "M" + def.name
                                        tpars = def.tpars |> List.collect (fun v -> ["v" + v; "n" + v])
                                }

                            let aType =
                                aDef |> TypeDef.instantiate (targs |> Array.toList |> List.collect (fun t ->
                                    let r = ofTypeRef needPrimaryKey simple false t
                                    if r.aSimple then
                                        [ TypeDef.imod |> TypeDef.instantiate [t]; t ]
                                    else
                                        let rr = ofTypeRef needPrimaryKey simple true t
                                        [ r.aType; rr.aType ]
                                ))

                            let args s =
                                targDesc |> List.collect (fun t ->
                                    if t.aSimple then
                                        []
                                    else
                                        [
                                            sprintf "(fun v -> %s)" (t.aInit s "v")
                                            sprintf "(fun (m,v) -> %s)" (t.aUpdate s "m" "v")
                                            sprintf "(fun v -> %s)" (t.aView s "v")
                                        ]
                                )



                            {
                                aSimple = false
                                aType = aType
                                aInit = fun s v ->
                                    let args = v :: args s |> String.concat ", "
                                    sprintf "%s.Create(%s)" (resettable.fullName s) args

                                aUpdate = fun s -> sprintf "%s.Update(%s, %s)" (resettable.fullName s)
                                aView = fun s v -> v
                            }

                        | Adaptive | Resettable ->
                            {
                                aSimple = true
                                aType = rt
                                aInit = fun _ v -> v
                                aUpdate = (fun _ _ _ -> "()")
                                aView = fun _ v -> v
                            }

                        | Unknown ->
                            {
                                aSimple = true
                                aType = TypeDef.imod |> TypeDef.instantiate [rt]
                                aInit = fun _ -> sprintf "ResetMod.Create(%s)"
                                aUpdate = fun _ -> sprintf "ResetMod.Update(%s,%s)"
                                aView = fun _ -> sprintf "%s :> IMod<_>"
                            }

        do Injected.injectedOfTypeRef <- ofTypeRef

module PreprocessingNew =
    open TypeTree

    let rec simpleOrNot (l : list<'a>) : list<list<'a * bool>> =
        match l with
            | [] -> [[]]
            | h :: t -> 
                simpleOrNot t |> List.collect (fun s ->
                    [(h, false) :: s; (h, true) :: s]
                )

    let generateLenses (e : FSharpEntity) =
        codegen {
            let! currentScope = CodeGen.currentScope
            let typeDef = TypeDef.ofFSharpDefinition ignore Map.empty e
            let pars = e.GenericParameters |> Seq.toList |> List.map (fun p -> p.Name)
            let typeRef = typeDef |> TypeDef.instantiate (List.map GenericParameter pars)

            let parDef = 
                match pars with
                    | [] -> ""
                    | _ -> pars |> List.map (sprintf "'%s") |> String.concat "," |> sprintf "<%s>"

            let fields =
                typeDef.fields

            if e.IsFSharpRecord then

                do! line "[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]"
                let mModule = scope "module %s =" typeDef.name
                do! mModule {
                    do! line "[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]"
                    let lensModule = scope "module Lens ="
                    do! lensModule {
                        let iName = typeRef.fullName currentScope
                        for f in fields do
                            let fName = f.name
                            let fType = f.fieldType.fullName currentScope

                            let lens = scope "let %s%s =" fName parDef
                            do! lens {
                                do! line "{ new Lens<%s, %s>() with" iName fType
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


    let generateMutableType (e : FSharpEntity) =
        codegen {
            let! currentScope = CodeGen.currentScope

            let err = System.Collections.Generic.List<TypeRef>()
            let needPrimaryKey (t : TypeRef) =
                err.Add t

            let typeDef = TypeDef.ofFSharpDefinition needPrimaryKey Map.empty e
            if e.IsFSharpRecord then
                let tpars = e.GenericParameters |> Seq.toList
                let ipars = tpars |> List.map (fun p -> p.Name)
                let iTypeRef = 
                    typeDef |> TypeDef.instantiate (List.map GenericParameter ipars)

                let genFields = typeDef.fields
                let fpars = ipars |> List.collect (fun v -> ["v" + v; "n" + v]) //genFields |> List.choose (fun (g,f) -> if g then Some f.name else None)
                        
                // if the type is generic we need a base-type for our implementations
                if typeDef.IsGeneric then
                    do! line "[<AbstractClass; StructuredFormatDisplay(\"{AsString}\")>]"
                    let margs = fpars |> Seq.map (sprintf "'%s") |> String.concat ","
                    do! line "type M%s<%s>() = " typeDef.name margs
                    do! push
                    for f in genFields do
                        do! line "abstract member %s : %s" f.name (f.description.aType.fullName currentScope)

                    do! line "abstract member AsString : string"
                    do! pop

                do! line ""
                do! line ""

                let creators = System.Collections.Generic.List<string>()

                for ass in simpleOrNot ipars do
                    let map = Map.ofList ass
                        
                    let fields =
                        let typeDef = TypeDef.ofFSharpDefinition needPrimaryKey map e
                        typeDef.fields

                    let suffix = ass |> List.map (fun (_,s) -> if s then "V" else "D") |> String.concat ""

                    let baseArgs =
                        ass |> List.collect (fun (t,s) ->
                            if s then [ sprintf "IMod<'%s>" t; "'" + t ]
                            else [ "'v" + t; "'v" + t ]
                        )

                    let pars =
                        ipars |> List.collect (fun p ->
                            if map.[p] then [p]
                            else [p; "m" + p; "v" + p]
                        )

                    let args =
                        [
                            yield "__initial", iTypeRef.fullName currentScope
                            for p in ipars do
                                if not map.[p] then
                                    yield sprintf "__%sinit" p, sprintf "'%s -> 'm%s" p p
                                    yield sprintf "__%supdate" p, sprintf "'m%s * '%s -> unit" p p
                                    yield sprintf "__%sview" p, sprintf "'m%s -> 'v%s" p p
                        ]

                    let argDef = args |> List.map (fun (n,t) -> sprintf "%s : %s" n t) |> String.concat ", "
                    let argRef = args |> List.map fst |> String.concat ", "

                    let selfName = 
                        match pars with
                            | [] -> "M" + typeDef.name
                            | _ -> sprintf "M%s%s<%s>" typeDef.name suffix (pars |> List.map (sprintf "'%s") |> String.concat ",")
                    let baseName = 
                        match pars with
                            | [] -> "obj"
                            | _ -> sprintf "M%s<%s>" typeDef.name (baseArgs |> String.concat ",")

                    let typePrefix =
                        match pars with
                            | [] ->  "type"
                            | _ -> "and private"

                    let mem =
                        match pars with
                            | [] -> "member"
                            | _ -> "override"

                    do! line "%s %s(%s) =" typePrefix selfName argDef
                    do! push
                    do! line "inherit %s()" baseName
     
                    do! line "let mutable __current = __initial"

                    // declare all the fields
                    for f in fields do
                        let i = sprintf "__initial.%s" f.name
                        if not f.nonIncremental then
                            if f.treatAsValue then
                                do! line "let _%s = ResetMod.Create(%s)" f.name i
                            else
                                do! line "let _%s = %s" f.name (f.description.aInit currentScope i)

                    do! line ""                   
                        
                    // declare the members
                    for f in fields do
                        if f.nonIncremental then
                            do! line "%s x.%s = __current.%s" mem f.name f.name
                        elif f.treatAsValue then
                            do! line "%s x.%s = _%s :> IMod<_>" mem f.name f.name
                        else
                            let name = sprintf "_%s" f.name
                            do! line "%s x.%s = %s" mem f.name (f.description.aView currentScope name)
                            
                    do! line ""
    
    
                    // define the update function
                    do! line "member x.Update(v : %s) =" (iTypeRef.fullName currentScope)
                    do! push
                    do! line "if not (System.Object.ReferenceEquals(__current, v)) then"
                    do! push
                    do! line "__current <- v"
                    do! line ""

                    for f in fields do
                        if f.nonIncremental then ()
                        elif f.treatAsValue then
                            do! line "_%s.Update(v.%s)" f.name f.name
                        else
                            let m = sprintf "_%s" f.name
                            let v = sprintf "v.%s" f.name

                            let code = f.description.aUpdate currentScope m v
                            for l in code.Split([|"\r\n"|], StringSplitOptions.RemoveEmptyEntries) do
                                do! line "%s" l

                    do! line ""
                    do! pop
                    do! pop
                    

                    do! line ""
                        
                    match pars with
                        | [] -> 
                            do! line "static member Create(%s) : %s = %s(%s)" argDef selfName selfName argRef
                        | _ ->
                            let ctargs = pars |> List.map (sprintf "'%s") |> String.concat ","
                            let creator = sprintf "static member Create<%s>(%s) : %s = %s(%s) :> %s" ctargs argDef baseName selfName argRef baseName
                            creators.Add creator

                    do! line "static member Update(m : %s, v : %s) = m.Update(v)" selfName (iTypeRef.fullName currentScope)
                    do! line ""
                    do! line "override x.ToString() = __current.ToString()"
                    do! line "%s x.AsString = sprintf \"%%A\" __current" mem

                    do! line "interface IUpdatable<%s> with" (iTypeRef.fullName currentScope)
                    do! push
                    do! line "member x.Update v = x.Update v"
                    do! pop

                    do! pop
                    do! line ""
                        
                if creators.Count > 0 then
                    do! line "and [<AbstractClass; Sealed>] M%s private() =" typeDef.name 
                    do! push

                    for c in creators do
                        do! line "%s" c
                        
                    let margs = fpars |> Seq.map (sprintf "'x%s") |> String.concat ","
                    let pars = ipars |> List.collect (fun p -> ["'" + p; "'xv" + p; "'xn" + p]) |> String.concat ","
                    do! line "static member Update<%s>(m : M%s<%s>, v : %s) : unit = " pars typeDef.name margs (iTypeRef.fullName currentScope)
                    do! push
                    do! line "match m :> obj with"
                    do! line "| :? IUpdatable<%s> as m -> m.Update(v)" (iTypeRef.fullName currentScope)
                    do! line "| _ -> failwith \"cannot update\""
                    do! pop

                    do! pop




                for e in err do
                    match e with
                        | Reference(def,_) ->
                            let r = def.range
                            do! warn 4321 r "%s should have a primary key since it's used in an aset (in the definition of %s)" def.name (typeDef.relativeName def)
                        | _ ->
                            ()

            elif e.IsFSharpUnion then
                if typeDef.IsGeneric then
                    do! error 4321 e.DeclarationLocation "generics not implementes yet: %s" (typeDef.fullName "")
                else
//                    let iTypeRef = typeDef |> TypeDef.instantiate []
//                    let mTypeRef = { typeDef with kind = Resettable; name = "M" + typeDef.name; path = "Mutable" :: typeDef.path }
//
//                    let cases = 
//                        e.UnionCases 
//                            |> Seq.toList
//                            |> List.map UnionCase.ofFSharpUnionCase
//
//                    do! line "[<AbstractClass; StructuredFormatDisplay(\"{AsString}\")>]"
//                    do! line "type %s() =" mTypeRef.name
//                    do! push
//
//                    do! line "abstract member TryUpdate : %s -> bool" iTypeRef.fullName
//                    do! line "abstract member AsString : string"
//
//                    do! line "static member CreateValue(v : %s) = " iTypeRef.fullName
//                    do! push
//                    do! line "match v with"
//                    
//                    for c in cases do
//                        let fieldPat = c.fields |> List.map (fun f -> 
//                        do! line "| %s() -> " c.name
//
//
//                    do! pop
//
//
//                    do! pop

                    ()
            else
                () //do! error 4321 e.DeclarationLocation "invalid domain-type: %s" typeDef.fullName
        }



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

    type DomainTypeReference =
        {
            creator : string -> string
            updater : string -> string -> string
        }

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

        let private rx = System.Text.RegularExpressions.Regex @"`[0-9]+"

        let rec immutableName (e : FSharpType) =
            if e.IsGenericParameter then
                e.GenericParameter.Name
            elif e.HasTypeDefinition then
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

            elif e.IsTupleType then
                e.GenericArguments |> Seq.map immutableName |> String.concat " * "
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

        let (|DomainType|_|) (t : FSharpType) =
            match t with
                | HSet _ -> Some t
                | HMap _ -> Some t
                | PList _ -> Some t
                | _ -> 
                    if FSharpType.isDomainType t then 
                        let def = t.TypeDefinition
                        Some (t)
                    else 
                        None
                
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
            aUpcast     : string -> string
            aInit       : string -> string
        }

    let valueAdapter =
        {
            aUpcast = fun v -> v + " :> IMod<_>"
            aInit = sprintf "ResetMod.Create(%s)"
        }
             
    type FSharpField with
        member x.CleanName = x.Name.ToLower()
      

    
    let rec domainTypeRef (prefix : string) (e : FSharpType) =
        if e.HasTypeDefinition then
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

            let name = 
                if path = "" then "M" + def.DisplayName
                else path + ".M" + def.DisplayName

            if e.GenericArguments.Count > 0 then
                    
                let nested = 
                    e.GenericArguments 
                        |> Seq.toList 
                        |> List.map (fun t ->
                            if FSharpType.isDomainType t then
                                let inner = domainTypeRef prefix t
                                "D", Some inner
                            else
                                "V", None
                        )

                let suffix = nested |> List.map fst |> String.concat ""
                let lambdas =
                    nested |> List.collect (fun (_,v) ->
                        match v with
                            | Some inner ->
                                [
                                    "(fun e -> " + inner.creator "e" + ")"
                                    "(fun (m,e) -> " + inner.updater "m" "e" + ")"
                                ]
                            | None ->
                                []
                    )

                let args = "__value" :: lambdas |> String.concat ","

                {
                    creator = fun v -> sprintf "%s.Create%s(%s)" name suffix (v :: lambdas |> String.concat ",")
                    updater = sprintf "%s.Update(%s, %s)" name
                }
            else


                {
                    creator = sprintf "%s.Create(%s)" name
                    updater = sprintf "%s.Update(%s, %s)" name
                }
        else
            failwithf "[Domain] no definition for %A" e 


    let rec generateAdapter (t : FSharpType) =
        codegen {
            let! scope = CodeGen.currentScope

            match t with
                | PList (DomainType t) ->
                    let ref = domainTypeRef scope t
                    
                    return {
                        aUpcast = fun v -> v + " :> alist<_>"
                        aInit = fun fName -> sprintf "ResetMapList(%s, (fun _ e -> %s), (fun (m,e) -> %s))"  fName (ref.creator "e") (ref.updater "m" "e")
                    }

                | PList t ->
                    return {
                        aUpcast = fun v -> v + " :> alist<_>"
                        aInit = sprintf "ResetList(%s)"
                    }

                | HSet (DomainType t) ->
                    let ref = domainTypeRef scope t
                    let mutable getKey = "unbox"
                    match tryGetUniqueField t with
                        | Some field ->
                            getKey <- sprintf "(fun (v : %s) -> v.%s :> obj)" t.TypeDefinition.FullName field
                        | None ->
                            let range = FSharpType.range t
                            do! warn 4321 range "the domain type %s has no field marked with PrimaryKeyAttribute but is used inside a hset. peformance could suffer. please consider adding a primary key." (FSharpType.prettyName t)
                 

                    return {
                        aUpcast = fun v -> v + " :> aset<_>"
                        aInit = fun fName -> sprintf "ResetMapSet(%s, %s, (fun e -> %s), (fun (m,e) -> %s))" getKey fName (ref.creator "e") (ref.updater "m" "e")
                    }

                | HSet t ->
                    return {
                        aUpcast = fun v -> v + " :> aset<_>"
                        aInit = fun fName -> sprintf "ResetSet(%s)" fName
                    }

                | HMap(k, DomainType t) ->
                    let ref = domainTypeRef scope t

     
                    return {
                        aUpcast = fun v -> v + " :> amap<_,_>"
                        aInit = fun fName -> sprintf "ResetMapMap(%s, (fun _ v -> %s), (fun (m,e) -> %s))" fName (ref.creator "v") (ref.updater "m" "e")
                    }

                | HMap(k, t) ->
                    return {
                        aUpcast = fun v -> v + " :> amap<_,_>"
                        aInit = fun fName -> sprintf "ResetMap(%s)" fName
                    }

            
                | Option (DomainType t) ->
                    let ref = domainTypeRef scope t


                    return {
                        aUpcast = fun v -> v + " :> IMod<_>"
                        aInit = fun fName -> sprintf "ResetMapOption(%s, (fun e -> %s), (fun (m,e) -> %s))" fName (ref.creator "e") (ref.updater "m" "e")
                    }
                       
                | Tuple types ->
                    let allDomain = types |> List.forall FSharpType.isDomainType
                    let noDomain = types |> List.forall (not << FSharpType.isDomainType)

                    let! adapters = types |> List.mapC generateAdapter

                    let names = adapters |> List.mapi (fun i _ -> sprintf "__item%d" i)
                    let def = String.concat "," names
                    let ref = List.map2 (fun a n -> a.aUpcast n) adapters names |> String.concat ","
                    let init = List.map2 (fun a n -> a.aInit n) adapters names |> String.concat ","

                    let upcaster (name : string) =
                        sprintf "let (%s) = %s in (%s)" def name ref

                    let init (name : string) =
                        sprintf "let (%s) = %s in (%s)" def name init
                        

                    return {
                        aUpcast = upcaster
                        aInit = init 
                    }

                | DomainType t ->
                    let ref = domainTypeRef scope t
                    return {
                        aUpcast = id
                        aInit = ref.creator
                    }


                | t -> 
                    return valueAdapter
             
        }

    let updateExpression (t : FSharpType) (mname : string) (iname : string) =
        let rec update (prefix : string) (t : FSharpType) (mname : string) (iname : string) =
            codegen {
                match t with
                    | Tuple types ->
                        let inames = types |> List.mapi (fun i _ -> sprintf "%s__v%d" prefix i)
                        let mnames = types |> List.mapi (fun i _ -> sprintf "%s__m%d" prefix i)

                        do! line "let (%s) = %s" (String.concat "," inames) iname
                        do! line "let (%s) = %s" (String.concat "," mnames) mname

                        for (t,m,i) in List.zip3 types mnames inames do
                            do! update m t m i

                    | _ ->
                        do! line "%s.Update(%s)" mname iname
            }

        update "" t mname iname

    

    let generateMutableType (e : FSharpEntity) =
        codegen {

            let immutableName = FSharpEntity.immutableName e
            let defName = FSharpEntity.mutableNameDef e
//            let targs = e.GenericParameters
//            if targs.Count > 0 then
//                let range = e.DeclarationLocation
//                do! error 5432 range "cannot compile generic domain type %s" e.DisplayName

            if e.IsFSharpRecord then
                do! PreprocessingNew.generateMutableType e
//                let! annotatedFields =
//                    e.FSharpFields
//                        |> Seq.toList
//                        |> List.chooseC (fun f ->
//                            codegen {
//                                if FSharpField.nonIncremental f then
//                                    return None
//                                else
//                                    let! adapter = 
//                                        if FSharpField.treatAsValue f then
//                                            codegen { return valueAdapter }
//                                        else
//                                            generateAdapter f.FieldType
//
//                                    let inputName = sprintf "__initial.%s" f.DisplayName
//                                    let init = sprintf "let _%s = %s" f.DisplayName (adapter.aInit inputName)
//
//                                    let access = adapter.aUpcast ("_" + f.DisplayName)
//
//                                    return Some (f.DisplayName, init, access)
//                            }
//                    )
//                    
//                match annotatedFields with
//                    | [] -> 
//                        ()
//                    | _ ->
//                        do! line "[<StructuredFormatDisplay(\"{AsString}\")>]"
//                        do! line "[<System.Runtime.CompilerServices.Extension>]"
//                        let typeDef = scope "type %s private(__initial : %s) =" defName immutableName
//                        do! typeDef {
//                            do! line "let mutable __current = __initial"
//
//                            for (_, init, _) in annotatedFields do
//                                do! line "%s" init
//                        
//                            do! line ""
//
//                            for (fname, _, access) in annotatedFields do
//                                do! line "member x.%s = %s" fname access
//
//                            for f in e.FSharpFields do
//                                if FSharpField.nonIncremental f then
//                                    do! line "member x.%s = __initial.%s" f.DisplayName f.DisplayName
//                    
//                            do! line ""
//
//                            
//                            let apply = scope "member x.Update(__model : %s) =" immutableName
//                            do! apply {
//                                do! line "if not (Object.ReferenceEquals(__model, __current)) then"
//                                do! push
//
//                                do! line "__current <- __model"
//                
//                                for f in e.FSharpFields do
//                                    if not (FSharpField.nonIncremental f) then
//                                        let fName = f.DisplayName
//
//                                        do! updateExpression f.FieldType ("_" + fName) ("__model." + fName)
//                                do! pop
//                            }
//                            do! line ""
//                            do! line "static member Update(__self : %s, __model : %s) = __self.Update(__model)" defName immutableName
//                            do! line ""
//                            do! line "static member Create(initial) = %s(initial)" defName
//                            do! line ""
//                            do! line "override x.ToString() = __current.ToString()"
//                            do! line "member private x.AsString = sprintf \"%%A\" __current"
//
//                        }
//
//                        do! line ""
//                        do! line ""
//
//
//                        do! line "[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]"
//                        let mModule = scope "module %s =" defName
//                        do! mModule {
//                            for (name,_,_) in annotatedFields do
//                                do! line "let inline %s (m : %s) = m.%s" name defName name
//                        }
//
//                        do! line ""
//                        do! line ""

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
                    do! line "    ResetMod.Create(%s.CreateValue v) :> IMod<_>" defName
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
                            let access = adapter.aUpcast ("_" + f.CleanName)
                            do! line "member x.%s = %s" f.CleanName access
                                        
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
                                let fName = f.CleanName

                                do! updateExpression f.FieldType ("_" + fName) fName
//
//                                match f.FieldType with
//                                    | Tuple types ->
//                                        let inames = types |> List.mapi (fun i _ -> sprintf "__v%d" i)
//                                        let mnames = types |> List.mapi (fun i _ -> sprintf "__m%d" i)
//
//                                        do! line "let (%s) = %s" (String.concat "," inames) fName
//                                        do! line "let (%s) = _%s" (String.concat "," mnames) fName
//
//                                        for (m,i) in List.zip mnames inames do
//                                            do! line "%s.Update(%s)" m i
//
//                                    | _ ->
//                                        do! line "_%s.Update(%s)" fName fName
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
                    do! PreprocessingNew.generateLenses e
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
