namespace Aardvark.Compiler.DomainTypes

open System
open System.IO
open Microsoft.Build.Framework
open Microsoft.Build.Utilities


type RealType =
    | System of string
    | Local of string

type TypeRef =
    | Value of RealType
    | Set of RealType

type RecordDef =
    { 
        name : string
        fields : list<string * TypeRef> 
    }

type UnionCaseDef =
    {
        name : string
        args : list<string * TypeRef>
    }

type UnionDef =
    { 
        name : string
        cases : list<UnionCaseDef> 
    }

type TypeDef =
    | Union of UnionDef
    | Record of RecordDef

type Definition = { ns : string; opens : list<string>; def : TypeDef }

module State =

    [<AbstractClass>]
    type State<'s, 'a>() =
        abstract member Run : byref<'s> -> 'a
        abstract member RunUnit : byref<'s> -> unit

        default x.Run(s) = x.RunUnit(&s); Unchecked.defaultof<'a>
        default x.RunUnit(s) = x.Run(&s) |> ignore

    module State =
            
        let ignore (s : State<'s, 'a>) =
            { new State<'s, unit>() with
                member x.RunUnit state =
                    s.RunUnit(&state)
            }

        let map (f : 'a -> 'b) (s : State<'s, 'a>) =
            { new State<'s, 'b>() with
                member x.Run state =
                    let a = s.Run(&state)
                    f a
            }

        let bind (f : 'a -> State<'s, 'b>) (s : State<'s, 'a>) =
            { new State<'s, 'b>() with
                member x.Run state =
                    let a = s.Run(&state) |> f
                    a.Run(&state)
            }

        let value (v : 'a) =
            { new State<'s, 'a>() with
                member x.Run _ = v
            }

        let get<'s> =
            { new State<'s, 's>() with
                member x.Run state = state
            }

        let put (s : 's) =
            { new State<'s, unit>() with
                member x.RunUnit state = state <- s
            }

        let modify (f : 's -> 's) =
            { new State<'s, unit>() with
                member x.RunUnit state = state <- f state
            }

        let custom (f : 's -> 's * 'a) =
            { new State<'s, 'a>() with
                member x.Run state = 
                    let (s,r) = f state
                    state <- s
                    r
            }

        let run (state : 's) (m : State<'s, 'a>) =
            let mutable res = state
            let v = m.Run(&res)
            res, v

        let evaluate (state : 's) (m : State<'s, 'a>) =
            let mutable res = state
            let v = m.Run(&res)
            v

    module Seq =
        let mapS (f : 'a -> State<'s, 'b>) (input : seq<'a>) =
            { new State<'s, seq<'b>>() with
                member x.Run state =
                    let result = System.Collections.Generic.List<'b>()
                    for e in input do
                        result.Add (f(e).Run(&state))
                    result :> seq<_>    
            }

        let collectS (f : 'a -> State<'s, seq<'b>>) (input : seq<'a>) =
            { new State<'s, seq<'b>>() with
                member x.Run state =
                    let result = System.Collections.Generic.List<'b>()
                    for e in input do
                        result.AddRange (f(e).Run(&state))
                    result :> seq<_>    
            }

        let chooseS (f : 'a -> State<'s, Option<'b>>) (input : seq<'a>) =
            { new State<'s, seq<'b>>() with
                member x.Run state =
                    let result = System.Collections.Generic.List<'b>()
                    for e in input do
                        match f(e).Run(&state) with
                            | Some res -> result.Add res
                            | _ -> ()
                    result :> seq<_>    
            }

    module List =
        let mapS (f : 'a -> State<'s, 'b>) (input : list<'a>) =
            { new State<'s, list<'b>>() with
                member x.Run state =
                    let result = System.Collections.Generic.List<'b>()
                    for e in input do
                        result.Add (f(e).Run(&state))
                    result |> Seq.toList
            }

        let collectS (f : 'a -> State<'s, seq<'b>>) (input : list<'a>) =
            { new State<'s, list<'b>>() with
                member x.Run state =
                    let result = System.Collections.Generic.List<'b>()
                    for e in input do
                        result.AddRange (f(e).Run(&state))
                    result |> Seq.toList
            }

        let chooseS (f : 'a -> State<'s, Option<'b>>) (input : list<'a>) =
            { new State<'s, list<'b>>() with
                member x.Run state =
                    let result = System.Collections.Generic.List<'b>()
                    for e in input do
                        match f(e).Run(&state) with
                            | Some res -> result.Add res
                            | _ -> ()
                    result |> Seq.toList
            }


    type StateBuilder() =
        member x.Bind(m : State<'s, 'a>, f : 'a -> State<'s, 'b>) =
            State.bind f m

        member x.Return(v : 'a) = 
            State.value v

        member x.ReturnFrom(m : State<'s, 'a>) =
            m

        member x.Delay(f : unit -> State<'s, 'a>) =
            { new State<'s, 'a>() with
                member x.Run(state) =
                    f().Run(&state)
            }

        member x.Combine(l : State<'s, unit>, r : State<'s, 'a>) =
            { new State<'s, 'a>() with
                member x.Run(state) =
                    l.Run(&state)
                    r.Run(&state)
            }

        member x.Zero() : State<'s, unit> =
            { new State<'s, unit>() with
                member x.RunUnit(state) =
                    ()
            }

        member x.For(input : seq<'a>, f : 'a -> State<'s, unit>) =
            { new State<'s, unit>() with
                member x.RunUnit(state) =
                    for e in input do
                        f(e).Run(&state)
            }

        member x.While(guard : unit -> bool, body : State<'s, unit>) =
            { new State<'s, unit>() with
                member x.RunUnit(state) =
                    while guard() do
                        body.Run(&state)
            }

        member x.TryFinally(m : State<'s, 'a>, fin : unit -> unit) =
            { new State<'s, 'a>() with
                member x.Run(state) =
                    try m.Run(&state)
                    finally fin()
            }

        member x.TryWith(m : State<'s, 'a>, handler : exn -> State<'s, 'a>) =
            { new State<'s, 'a>() with
                member x.Run(state) =
                    let mutable local = state
                    try 
                        let res = m.Run(&local)
                        state <- local
                        res
                    with e ->  
                        handler(e).Run(&state)
            }

        member x.Using(v : 'a, f : 'a -> State<'s, 'b>) =
            { new State<'s, 'b>() with
                member x.Run(state) = 
                    use v = v
                    let res = f(v).Run(&state)
                    res
            }

        member x.TryFinally(m : State<'s, 'a>, fin : unit -> State<'s, unit>) =
            { new State<'s, 'a>() with
                member x.Run(state) =
                    try m.Run(&state)
                    finally fin().Run(&state)
            }

        member x.TryFinally(m : State<'s, 'a>, fin : unit -> 's -> 's) =
            { new State<'s, 'a>() with
                member x.Run(state) =
                    try m.Run(&state)
                    finally state <- fin () state
            }

    let state = StateBuilder()


module Generator =
    open System.Text
    open State

    type GeneratorMode =
        | Immutable
        | Mutable

    type GeneratorState =
        {
            builder : StringBuilder
            indent : string
            mode : GeneratorMode
        }

    module Generator =
        
        let push =
            State.modify (fun s -> { s with indent = s.indent + "    " })

        let pop = 
            State.modify (fun s -> 
                if s.indent.Length >= 4 then
                    { s with indent = s.indent.Substring(4) }
                else
                    { s with indent = "" } 
            )
                
        let line fmt = 
            let writeLine str =
                state {
                    let! s = State.get
                    s.builder.AppendLine(s.indent + str) |> ignore
                }
            Printf.kprintf writeLine fmt

        let mode = State.get |> State.map (fun s -> s.mode)

        let run (mode : GeneratorMode) (s : State<GeneratorState, unit>) =
            let mutable state =
                {
                    builder = StringBuilder()
                    indent = ""
                    mode = mode
                }
            s.RunUnit(&state)
            state.builder.ToString()
            
    let generator = StateBuilder()

    let rec realTypeString (mode : GeneratorMode) (r : RealType) =
        match r with
            | System n -> n
            | Local n -> 
                match mode with
                    | Immutable -> n
                    | Mutable -> sprintf "M%s" n

    let rec typeString (t : TypeRef) =
        generator {
            let! m = Generator.mode

            match t with
                | Value rt -> 
                    let isLocal = match rt with | Local _ -> true | _ -> false
                    let rt = realTypeString m rt
                    match m with
                        | Immutable -> return rt
                        | Mutable -> 
                            if isLocal then return rt
                            else return sprintf "ModRef<%s>" rt

                | Set (Local name) -> 
                    match m with
                        | Immutable -> 
                            return sprintf "pset<%s>" name

                        | Mutable -> 
                            return sprintf "MapSet<%s, M%s>" name name

                | Set (System name) -> 
                    match m with
                        | Immutable -> 
                            return sprintf "pset<%s>" name

                        | Mutable -> 
                            return sprintf "ResetSet<%s>" name

        }

    let generateTypes (defs : list<list<string> * TypeDef>) =
        generator {

            let! mode = Generator.mode
            let prefix =
                match mode with
                    | Mutable -> "m"
                    | Immutable -> ""

            for (opens, d) in defs do
                for o in opens do
                    do! Generator.line "open %s" o
                do! Generator.line ""

                match d with
                    | Record d ->
                        let name = realTypeString mode (Local d.name)
                        let iName = realTypeString Immutable (Local d.name)

                        do! Generator.line "type %s =" name
                        do! Generator.push

                        do! Generator.line "{"
                        do! Generator.push

                        match mode with
                            | Immutable -> do! Generator.line "mutable _id : Id"
                            | Mutable -> do! Generator.line "mutable original : %s" iName

                        for (n,t) in d.fields do
                    
                            let! tn = typeString t
                            do! Generator.line "%s%s : %s" prefix n tn
                
                        do! Generator.pop
                        do! Generator.line "}"

                        match mode with
                            | Immutable ->
                                do! Generator.line "interface IUnique with"
                                do! Generator.line "    member x.Id "
                                do! Generator.line "        with get() = x._id"
                                do! Generator.line "        and set v = x._id <- v"

                                do! Generator.line "static member Empty = "
                                do! Generator.push
                                do! Generator.line "{"
                                do! Generator.push

                                do! Generator.line "_id = null"
                                for (n,t) in d.fields do
                                    let! tn = typeString t
                                    do! Generator.line "%s = Unchecked.defaultof<%s>" n tn

                                do! Generator.pop
                                do! Generator.line "}"
                                do! Generator.pop

                            | _ ->
                                ()

                        do! Generator.pop
                        do! Generator.line ""

                    | Union d ->
                        let name = realTypeString mode (Local d.name)
                        do! Generator.line "type %s =" name
                        do! Generator.push

                        let casePrefix = prefix.ToUpper()
                        for c in d.cases do
                            let! all = c.args |> List.mapS (fun (n,t) -> generator { let! tn = typeString t in return n,tn })
                            let args = all |> List.map (fun (n,t) -> sprintf "%s : %s" n t) |> String.concat " * "
                            if args.Length > 0 then
                                do! Generator.line "| %s%s of %s" casePrefix c.name args
                            else
                                do! Generator.line "| %s%s" casePrefix c.name

                        do! Generator.pop
                        do! Generator.line ""

        }

    let generateDiffs (defs : list<list<string> * TypeDef>) =
        generator {
            do! Generator.line "open System.Runtime.CompilerServices"
            do! Generator.line "[<AbstractClass; Sealed; Extension>]"
            do! Generator.line "type Differential private() = "
            do! Generator.push

            for (opens, d) in defs do

                match d with
                    | Record d ->
                        let mName = realTypeString Mutable (Local d.name)
                        let iName = realTypeString Immutable (Local d.name)

                        do! Generator.line "[<Extension>]"
                        do! Generator.line "static member ToMod(l : %s, cache : ReuseCache) = " iName
                        do! Generator.push

                        do! Generator.line "{"
                        do! Generator.push

                        do! Generator.line "original = l"

                        for (n,t) in d.fields do
                            match t with
                                | Value t ->
                                    match t with
                                        | Local _ ->
                                            do! Generator.line "m%s = Differential.ToMod(l.%s, cache)" n n
                                        | _ ->
                                            do! Generator.line "m%s = Mod.init l.%s" n n
                                | Set t ->
                                    match t with
                                        | Local _ ->
                                            do! Generator.line "m%s = MapSet(cache.GetCache(), l.%s, (fun a -> Differential.ToMod(a, cache)), (fun (v,k) -> Differential.Apply(cache, v, k)))" n n
                                        | _ ->
                                            do! Generator.line "m%s = ResetSet(l.%s)" n n
                                    
                            
                                    
                            ()

                        do! Generator.pop
                        do! Generator.line "}"

                        do! Generator.pop
                        do! Generator.line ""

                    | Union d ->
                        let mName = realTypeString Mutable (Local d.name)
                        let iName = realTypeString Immutable (Local d.name)
                        do! Generator.line "static member ToMod(l : %s, cache : ReuseCache) : %s = " iName mName
                        do! Generator.push
                        do! Generator.line "failwith \"not implemented\""
                        do! Generator.pop
                        do! Generator.line ""

            for (opens, d) in defs do
                match d with
                    | Record d ->
                        let mName = realTypeString Mutable (Local d.name)
                        let iName = realTypeString Immutable (Local d.name)
                        
                        do! Generator.line "[<Extension>]"
                        do! Generator.line "static member Apply(cache : ReuseCache, r : %s, l : %s) = " mName iName
                        do! Generator.push
                        do! Generator.line "if not (System.Object.ReferenceEquals(l, r.original)) then"
                        do! Generator.push
                        do! Generator.line "r.original <- l"


                        for (n,t) in d.fields do
                            match t with
                                | Value pt ->
                                    match pt with
                                        | Local _ ->
                                            do! Generator.line "Differential.Apply(r.m%s, l.%s)" n n
                                        | _ ->
                                            do! Generator.line "Mod.change r.m%s l.%s" n n 
                                | Set pt ->
                                    match pt with
                                        | Local _ ->
                                            do! Generator.line "r.m%s.Update(l.%s)" n n
                                        | _ ->
                                            do! Generator.line "r.m%s.Update(l.%s)" n n 
                    
                            ()

                        do! Generator.pop
                        do! Generator.pop
                        do! Generator.line ""

                    | Union d ->
                        let mName = realTypeString Mutable (Local d.name)
                        let iName = realTypeString Immutable (Local d.name)

                        do! Generator.line "static member Apply(cache : ReuseCache, r : %s, l : %s) : unit = " mName iName
                        do! Generator.push
                        do! Generator.line "failwith \"not implemented\""
                        do! Generator.pop
                        do! Generator.line ""
                        

                ()

            do! Generator.pop
            do! Generator.line ""
        }

    let generate (defs : list<Definition>) =
        let defs =  
            defs 
                |> Seq.groupBy (fun d -> d.ns)
                |> Seq.map (fun (ns, defs) -> ns, defs |> Seq.map (fun d -> d.opens, d.def) |> Seq.toList)
                |> Seq.toList
        let gen = 
            generator {
                for (ns, defs) in defs do
                    do! Generator.line "namespace %s" ns
                    do! Generator.line "[<AutoOpen>]"
                    do! Generator.line "module Generated ="
                    do! Generator.push
                    do! Generator.line ""
                    do! Generator.line "open %s" ns
                    do! Generator.line "open Aardvark.Base.Incremental"
                    do! Generator.line ""

                    do! State.modify (fun s -> { s with mode = Immutable })
                    do! generateTypes defs

                    do! State.modify (fun s -> { s with mode = Mutable })
                    do! generateTypes defs

                    do! generateDiffs defs
                    do! Generator.pop

            }
        Generator.run Immutable gen 


module private Preprocessing =
    open System.Collections.Generic
    open Microsoft.FSharp.Compiler.Ast
    open Microsoft.FSharp.Compiler.SourceCodeServices
    open Microsoft.FSharp.Compiler.Range
    let checker = FSharpChecker.Create()


    let idString (ids : LongIdent) =
        ids |> List.map (fun i -> i.idText) |> String.concat "."

    let lastString (LongIdentWithDots(ids, _)) =
        let v = ids |> List.last
        v.idText

    type MiniType =
        | Anonymous
        | Leaf of string
        | Array of int * MiniType
        | Tuple of list<MiniType>
        | Generic of MiniType * list<MiniType>
        | Fun of MiniType * MiniType

    let rec toMiniType (t : SynType) : MiniType =
        match t with
            | SynType.Array(dim, t, _) ->
                let inner = toMiniType t
                Array(dim, inner)

            | SynType.Fun(arg, ret,_) ->
                let a = toMiniType arg
                let r = toMiniType ret
                Fun(a,r)

            | SynType.App(n, _, args,_, _, _, _) ->
                let n = toMiniType n
                let args = args |> List.map toMiniType
                Generic(n, args)

            | SynType.Tuple(args, _) ->
                let args = args |> List.map (snd >> toMiniType)
                Tuple(args)

            | SynType.LongIdent(LongIdentWithDots(id,_)) ->
                let n = idString id
                Leaf(n)

            | SynType.HashConstraint(t,_) ->
                toMiniType t

            | SynType.LongIdentApp(n, _, _, args, _, _, _) ->
                let n = toMiniType n
                let args = args |> List.map toMiniType
                Generic(n, args)
                
            | SynType.Anon _ ->
                Anonymous

            | SynType.WithGlobalConstraints(t,_,_) ->
                toMiniType t

            | _ ->  
                failwith ""

    let rec miniName (t : MiniType) =
        match t with
            | Anonymous -> "_"
            | Leaf n -> n
            | Array(dim, t) -> 
                let commas = System.String(Array.init (dim-1) (fun _ -> ','))
                sprintf "(%s)[%s]" (miniName t) commas
            | Tuple(args) ->
                args |> List.map (miniName >> sprintf "(%s)") |> String.concat " * "
            | Generic(n, args) ->
                let n = miniName n
                let args = args |> List.map miniName |> String.concat ", "
                sprintf "%s<%s>" n args
            | Fun(l,r) ->
                let l = l |> miniName
                let r = r |> miniName
                sprintf "(%s) -> (%s)" l r

    let toRealType (locals : Set<string>) (t : MiniType) =
        let n = miniName t
        if Set.contains n locals then
            Local n
        else
            System n
        
    let rec toTypeRef (locals : Set<string>) (t : MiniType) =
        match t with
            | Generic(Leaf "pset", [arg]) -> 
                Set(toRealType locals arg)
            | t ->
                Value(toRealType locals t)


    let toAst (log : TaskLoggingHelper) (locals : Set<string>) (name : string) (def : SynTypeDefnSimpleRepr) =
        match def with
            | SynTypeDefnSimpleRepr.Record(_,fields,_) ->
                let fields =
                    fields |> List.map (fun (SynField.Field(_,_,id,t,_,_,_,_)) ->
                        let mini = toMiniType t

                        id.Value.idText, toTypeRef locals mini
                    )

                Record { name = name; fields = fields }

            | SynTypeDefnSimpleRepr.Union(_,cases,_) ->
                log.LogError("union types are currently not supported as domain-types")
                failwith ""

            | _ ->
                log.LogError(sprintf "types of kind %A types are currently not supported as domain-types" def)
                failwith ""

    let rec findDomTypes (log : TaskLoggingHelper) (ns : string) (opened : ref<list<string>>) (locals : Dictionary<string, HashSet<string>>) (d : SynModuleDecl) =
        match d with
            | SynModuleDecl.NestedModule(comp,_,decls,_,_) ->
                let (SynComponentInfo.ComponentInfo(attributes, typeParams, _, typeId, _, _, _, _)) = comp
                let name = idString typeId
                let ns = ns + "." + name
                let inner = ref !opened
                decls |> List.collect (findDomTypes log ns inner locals)

            | SynModuleDecl.Open(LongIdentWithDots(name, _),_) ->
                let str = idString name
                opened := str::!opened
                []

            | SynModuleDecl.Types(defs, _) ->
                defs |> List.collect (fun (TypeDefn(comp, repr, members, _)) ->
                    let (SynComponentInfo.ComponentInfo(attributes, typeParams, _, typeId, _, _, _, _)) = comp
                    let name = idString typeId

                    let isDomainType =
                        attributes |> List.exists (fun a -> 
                            let n = lastString a.TypeName
                            n = "DomainType" || n = "DomainTypeAttribute"
                        )

                    if isDomainType then
                        let current =
                            match locals.TryGetValue(ns) with
                                | (true, set) -> set
                                | _ ->
                                    let set = HashSet()
                                    locals.[ns] <- set
                                    set

                        current.Add name |> ignore

                        let importedLocals =
                            !opened |> Seq.collect (fun ns ->
                                match locals.TryGetValue ns with
                                    | (true, v) -> v :> seq<_>
                                    | _ -> Seq.empty
                            )

                        let visibleLocals = Set.union (Set.ofSeq current) (Set.ofSeq importedLocals)

                        match repr with
                            | SynTypeDefnRepr.Simple(simple,_) ->
                                [ { ns = ns; opens = List.rev !opened; def = toAst log visibleLocals name simple }]
                                //[!opened, ns, name, toAst visibleLocals name simple]
                            | _ ->
                                failwith ""
                    else
                        []
                )

            | _ -> []

    let findDomainTypes (log : TaskLoggingHelper) (ast : ParsedInput) =
        match ast with
            | ParsedInput.ImplFile(ParsedImplFileInput(_, _, _, _, _, modulesAndNamespaces,_)) ->
                let locals = Dictionary()
                modulesAndNamespaces |> List.collect (fun (SynModuleOrNamespace(id,_,_,decls,_,_,_,_)) ->
                    let ns = idString id
                    let opened = ref []

                    decls |> List.collect (findDomTypes log ns opened locals)
                )
            | _ ->
                []

    
    open System.Runtime.CompilerServices

    type SynComponentInfo with
        member x.Name =
            match x with
                | SynComponentInfo.ComponentInfo(_,_,_,id,_,_,_,_) ->
                    idString id

        member x.Attributes =
            match x with
                | SynComponentInfo.ComponentInfo(att,_,_,_,_,_,_,_) -> att

    type SynTypeDefn with
        member x.Name =
            match x with
                | SynTypeDefn.TypeDefn(ci,_,_,_) -> ci.Name

        member x.IsDomainType =
            match x with
                | SynTypeDefn.TypeDefn(ci,_,_,_) -> 
                    ci.Attributes |> List.exists (fun a -> 
                        let n = lastString a.TypeName
                        n = "DomainType" || n = "DomainTypeAttribute"
                    )
      
    type SynType with
        member x.NameOption =
            match x with
                | SynType.LongIdent(LongIdentWithDots(id,_)) -> idString id |> Some
                | SynType.HashConstraint(t,_) -> t.NameOption
                | SynType.LongIdentApp(n, _, _, args, _, _, _) -> n.NameOption
                | SynType.WithGlobalConstraints(t,_,_) -> t.NameOption
                | _ -> None              


    let (|PSet|MapSet|ResetSet|ModRef|Other|) (t : SynType) =
        match t with
            | SynType.App(n, _, args, _, _, _, _) ->
                match n.NameOption, args with
                    | Some "pset", [a] -> PSet a
                    | Some "MapSet", [a;b] -> MapSet(a,b)
                    | Some "ResetSet", [a] -> ResetSet(a)
                    | Some "ModRef", [a] -> ModRef(a)
                    | _ -> Other
            | _ ->
                Other
        
    type SynExpr with
        static member Seq (l : list<SynExpr>) =
            let rec build (l : list<SynExpr>) =
                match l with
                    | [] -> SynExpr.Const(SynConst.Unit, range0)
                    | [e] -> e
                    | h :: rest ->
                        SynExpr.Sequential(SequencePointInfoForSeq.SequencePointsAtSeq, true, h, build rest, range0)

            build l

    module SynIndexerArg =
        let map (f : SynExpr -> SynExpr) (m : SynIndexerArg) =
            match m with
                | SynIndexerArg.One(a) -> SynIndexerArg.One(f a)
                | SynIndexerArg.Two(a, b) -> SynIndexerArg.Two(f a, f b)
    module SynBinding =
        let map (f : SynExpr -> SynExpr) (m : SynBinding) =
            let (SynBinding.Binding(access,kind,mustInline,isMutable,attributes,xmlDoc,synVal,pattern,retInfo,rhs,range,seqInfo)) = m
            SynBinding.Binding(access, kind, mustInline, isMutable, attributes, xmlDoc, synVal, pattern, retInfo, f rhs, range, seqInfo)

    module SynMatchClause =
        let map (f : SynExpr -> SynExpr) (m : SynMatchClause) =
            let (SynMatchClause.Clause(a,b,c,d,e)) = m
            SynMatchClause.Clause(a, Option.map f b, f c, d, e)

    module SynInterfaceImpl =
        let map (f : SynExpr -> SynExpr) (m : SynInterfaceImpl) =
            let (SynInterfaceImpl.InterfaceImpl(a,b,c)) = m
            SynInterfaceImpl.InterfaceImpl(a, List.map (SynBinding.map f) b, c)

    let rec substitute (f : SynExpr -> Option<SynExpr>) (e : SynExpr) =
        match f e with
            | Some res -> res
            | None ->
                let substitute = substitute f
                match e with
                    | SynExpr.AddressOf(a,e,b,c)            -> SynExpr.AddressOf(a, substitute e, b, c)
                    | SynExpr.App(a,b,c,d,e)                -> SynExpr.App(a,b,substitute c,substitute d,e)
                    | SynExpr.ArrayOrList(a,b,c)            -> SynExpr.ArrayOrList(a, b |> List.map (substitute), c)
                    | SynExpr.ArrayOrListOfSeqExpr(a,b,c)   -> SynExpr.ArrayOrListOfSeqExpr(a,substitute b,c)
                    | SynExpr.Assert(a,b)                   -> SynExpr.Assert(substitute a, b)
                    | SynExpr.CompExpr(a,b,c,d)             -> SynExpr.CompExpr(a, b, substitute c, d)
                    | SynExpr.DiscardAfterMissingQualificationAfterDot(a,b) -> SynExpr.DiscardAfterMissingQualificationAfterDot(substitute a,b)
                    | SynExpr.Do(a,b)                       -> SynExpr.Do(substitute a, b)
                    | SynExpr.DoBang(a,b)                   -> SynExpr.DoBang(substitute a, b)
                    | SynExpr.DotGet(a,b,c,d)               -> SynExpr.DotGet(substitute a,b,c,d)
                    | SynExpr.DotIndexedGet(a,b,c,d)        -> SynExpr.DotIndexedGet(substitute a,List.map (SynIndexerArg.map (substitute)) b,c,d)
                    | SynExpr.DotIndexedSet(a,b,c,d,e,f)    -> SynExpr.DotIndexedSet(substitute a, List.map (SynIndexerArg.map (substitute)) b, substitute c, d, e, f)
                    | SynExpr.DotNamedIndexedPropertySet(a,b,c,d,e) -> SynExpr.DotNamedIndexedPropertySet(substitute a, b, substitute c, substitute d, e)

                    | SynExpr.DotSet(a,b,c,d)               -> SynExpr.DotSet(substitute a, b, substitute c, d)
                    | SynExpr.Downcast(a,b,c)               -> SynExpr.Downcast(substitute a, b, c)
                    | SynExpr.Fixed(a,b)                    -> SynExpr.Fixed(substitute a, b)
                    | SynExpr.For(a,b,c,d,e,f,g)            -> SynExpr.For(a, b, substitute c, d, substitute e, substitute f, g)
                    | SynExpr.ForEach(a,b,c,d,e,f,g)        -> SynExpr.ForEach(a, b, c, d, substitute e, substitute f, g)
                    | SynExpr.FromParseError(a,b)           -> SynExpr.FromParseError(substitute a, b)
                    | SynExpr.IfThenElse(a,b,c,d,e,f,g)     -> SynExpr.IfThenElse(substitute a, substitute b, Option.map substitute c, d, e, f, g)
                    | SynExpr.InferredDowncast(a,b)         -> SynExpr.InferredDowncast(substitute a, b)
                    | SynExpr.InferredUpcast(a,b)           -> SynExpr.InferredUpcast(substitute a, b)
                    | SynExpr.JoinIn(a,b,c,d)               -> SynExpr.JoinIn(substitute a, b, substitute c, d)
                    | SynExpr.Lambda(a,b,c,d,e)             -> SynExpr.Lambda(a,b,c,substitute d,e)
                    | SynExpr.Lazy(a,b)                     -> SynExpr.Lazy(substitute a, b)
                    | SynExpr.LetOrUse(a,b,c,d,e)           -> SynExpr.LetOrUse(a, b, List.map (SynBinding.map substitute) c, substitute d, e)
                    | SynExpr.LetOrUseBang(a,b,c,d,e,f,g)   -> SynExpr.LetOrUseBang(a, b, c, d, substitute e, substitute f, g)
                    | SynExpr.LongIdentSet(a,b,c)           -> SynExpr.LongIdentSet(a, substitute b, c)
                    | SynExpr.Match(a,b,c,d,e)              -> SynExpr.Match(a, substitute b, List.map (SynMatchClause.map substitute) c, d, e)
                    | SynExpr.MatchLambda(a,b,c,d,e)        -> SynExpr.MatchLambda(a, b, List.map (SynMatchClause.map substitute) c, d, e)
                    | SynExpr.NamedIndexedPropertySet(a,b,c,d) -> SynExpr.NamedIndexedPropertySet(a, substitute b, substitute c, d)
                    | SynExpr.New(a,b,c,d)                  -> SynExpr.New(a, b, substitute c, d)
                    | SynExpr.ObjExpr(a,b,c,d,e,f)          -> SynExpr.ObjExpr(a, b |> Option.map (fun (a,b) -> (substitute a,b)), List.map (SynBinding.map substitute) c , List.map (SynInterfaceImpl.map substitute) d, e, f) 
                    | SynExpr.Paren(a,b,c,d)                -> SynExpr.Paren(substitute a, b, c, d)
                    | SynExpr.Quote(a,b,c,d,e)              -> SynExpr.Quote(substitute a, b, substitute c, d, e)
                    
                    | SynExpr.Record(a,b,c,d)               ->
                        let a =
                            match a with
                                | Some(a,b,c,d,e) -> Some(a, substitute b,c,d,e)
                                | None -> None
                        let b =
                            match b with
                                | Some(a,b) -> Some(substitute a, b)
                                | None -> None
                        let c =
                            c |> List.map (fun (a,b,c) ->
                                a, Option.map substitute b, c
                            )
                        SynExpr.Record(a,b,c,d)

                    | SynExpr.Sequential(a,b,c,d,e)         -> SynExpr.Sequential(a, b, substitute c, substitute d, e)
                    | SynExpr.TraitCall(a,b,c,d)            -> SynExpr.TraitCall(a, b, substitute c, d)
                    | SynExpr.TryFinally(a,b,c,d,e)         -> SynExpr.TryFinally(substitute a, substitute b, c, d, e)
                    | SynExpr.TryWith(a,b,c,d,e,f,g)        -> SynExpr.TryWith(substitute a, b, List.map (SynMatchClause.map substitute) c, d, e, f, g)
                    | SynExpr.Tuple(a,b,c)                  -> SynExpr.Tuple(List.map substitute a, b, c)
                    | SynExpr.TypeApp(a,b,c,d,e,f,g)        -> SynExpr.TypeApp(substitute a, b, c, d, e, f, g)
                    | SynExpr.Typed(a,b,c)                  -> SynExpr.Typed(substitute a, b, c)
                    | SynExpr.TypeTest(a,b,c)               -> SynExpr.TypeTest(substitute a, b, c)
                    | SynExpr.Upcast(a,b,c)                 -> SynExpr.Upcast(substitute a, b, c)
                    | SynExpr.While(a,b,c,d)                -> SynExpr.While(a, substitute b, substitute c, d)
                    | SynExpr.YieldOrReturn(a,b,c)          -> SynExpr.YieldOrReturn(a, substitute b, c)
                    | SynExpr.YieldOrReturnFrom(a,b,c)      -> SynExpr.YieldOrReturnFrom(a, substitute b, c)


                    | SynExpr.LibraryOnlyILAssembly _
                    | SynExpr.LibraryOnlyStaticOptimization _
                    | SynExpr.LibraryOnlyUnionCaseFieldGet _
                    | SynExpr.LibraryOnlyUnionCaseFieldSet _ ->
                        e
                    
                    | SynExpr.Null _
                    | SynExpr.LongIdent _ 
                    | SynExpr.ImplicitZero _
                    | SynExpr.Ident _
                    | SynExpr.Const _ 
                    | SynExpr.ArbitraryAfterError _ -> 
                        e
                  
    [<AbstractClass; Sealed; Extension>]
    type Substitution private() =

        static member LiftRecordConstructions(e : SynExpr) =
            let replaceNewRecord (e : SynExpr) =
                match e with
                    | SynExpr.Record(None,None,fields,d) ->
                        let id = LongIdentWithDots([Ident("_id", range0)], [range0]), true
                        let e = SynExpr.Null(range0)
                        let sep : BlockSeparator = range0, None

                        let fields = 
                            fields |> List.map (fun (id,e,s) -> 
                                match e with
                                    | Some e -> (id, Substitution.LiftRecordConstructions(e) |> Some, s)
                                    | None -> (id, None, s)
                            )

                        let fields = (id, Some e, Some sep) :: fields
                        SynExpr.Record(None, None, fields, d) |> Some
                    | _ ->
                        None

            substitute replaceNewRecord e


        [<Extension>]
        static member SubstituteTypeDef (ast : list<SynModuleDecl>, ns : string, f : string -> list<SynTypeDefn> -> list<SynTypeDefn>) : list<SynModuleDecl> =
            let rec substituteSingle (ns : string) (m : SynModuleDecl) =
                match m with
                    | SynModuleDecl.NestedModule(a,b,decls,d,e) ->
                        let name = a.Name
                        SynModuleDecl.NestedModule(a, b, substitute (sprintf "%s.%s" ns name) decls, d, e)

                    | SynModuleDecl.Types(defs, range) ->
                        SynModuleDecl.Types(f ns defs, range)

                    | SynModuleDecl.Let(isRec, bindings, range) ->
                        let newBindings =
                            bindings |> List.map (fun (SynBinding.Binding(access,kind,mustInline,isMutable,attributes,xmlDoc,synVal,pattern,retInfo,rhs,range,seqInfo)) ->
                                let rhs = Substitution.LiftRecordConstructions(rhs)
                                SynBinding.Binding(access,kind,mustInline,isMutable,attributes,xmlDoc,synVal,pattern,retInfo,rhs,range,seqInfo)
                            )

                        SynModuleDecl.Let(isRec, newBindings, range)

                    | _ ->
                        m

            and substitute (ns : string) (m : list<SynModuleDecl>) =
                match m with
                    | [] -> []
                    | m :: rest ->
                        let newHead = substituteSingle ns m
                        newHead :: substitute ns rest

            substitute ns ast

        [<Extension>]
        static member SubstituteTypeDef (ast : SynModuleOrNamespace, f : string -> list<SynTypeDefn> -> list<SynTypeDefn>) : SynModuleOrNamespace =
            let (SynModuleOrNamespace(id,a,b,decls,c,d,e,ff)) = ast
            let name = idString id

            let newDecl = 
                let newDecls = Substitution.SubstituteTypeDef(decls,name, f)
                let att =
                    { 
                        SynAttribute.TypeName = LongIdentWithDots([Ident("AutoOpen", range0)], [])
                        SynAttribute.ArgExpr = SynExpr.Const(SynConst.Unit, range0)
                        SynAttribute.Target = None
                        SynAttribute.AppliesToGetterAndSetter = false
                        SynAttribute.Range = range0
                    }

                SynModuleDecl.NestedModule(SynComponentInfo.ComponentInfo([att], [], [], [Ident("Generated", range0)], PreXmlDocEmpty, false, None, range0), false, newDecls, false, range0)

            SynModuleOrNamespace(id, a, b, [newDecl], c, d, e, ff)

        [<Extension>]
        static member SubstituteTypeDef (ast : ParsedInput, f : string -> list<SynTypeDefn> -> list<SynTypeDefn>) =
            match ast with
                | ParsedInput.ImplFile(ParsedImplFileInput(fileName, isScript, qualName, scopedPragma, parsedHashDirectives, modulesAndNamespaces, dunno)) ->
                    ParsedInput.ImplFile(ParsedImplFileInput(fileName, isScript, qualName, scopedPragma, parsedHashDirectives, modulesAndNamespaces |> List.map (fun m -> Substitution.SubstituteTypeDef(m,f)), dunno))
                | ast ->
                    ast
 
        [<Extension>]       
        static member GetAllDomainTypes (m : list<SynModuleDecl>, ns : string) =
            match m with
                | [] -> Set.empty
                | h :: rest ->
                    match h with
                        | SynModuleDecl.Types(types,_) ->
                            let all =
                                types |> List.choose (fun (SynTypeDefn.TypeDefn(SynComponentInfo.ComponentInfo(att,_,_,id,_,_,_,_),repr,c,d)) ->
                                    let name = idString id
                                    let isDomainType =
                                        att |> List.exists (fun a -> 
                                            let n = lastString a.TypeName
                                            n = "DomainType" || n = "DomainTypeAttribute"
                                        )

                                    if isDomainType then
                                        match repr with
                                            | SynTypeDefnRepr.Simple(SynTypeDefnSimpleRepr.Record(_,fields,_), _) -> Some (ns, name)
                                            | _ -> None
                                    else
                                        None
                                )                            

                            let s = Set.ofList all
                            Set.union s (Substitution.GetAllDomainTypes(rest, ns))

                        | SynModuleDecl.NestedModule(SynComponentInfo.ComponentInfo(att,_,_,id,_,_,_,_), _, decls, _, _) ->
                            let name = idString id
                            let inner = Substitution.GetAllDomainTypes(decls, sprintf "%s.%s" ns name)

                            Set.union inner (Substitution.GetAllDomainTypes(rest, ns))

                        | _ ->
                            Substitution.GetAllDomainTypes(rest, ns)

        [<Extension>]
        static member GetAllDomainTypes (m : SynModuleOrNamespace) =
            let (SynModuleOrNamespace(id,a,b,decls,c,d,e,ff)) = m
            Substitution.GetAllDomainTypes(decls, idString id)
   
        [<Extension>] 
        static member GetAllDomainTypes (ast : ParsedInput) =
            match ast with
                | ParsedInput.ImplFile(ParsedImplFileInput(fileName, isScript, qualName, scopedPragma, parsedHashDirectives, modulesAndNamespaces, dunno)) ->
                    modulesAndNamespaces |> List.map (fun m -> Substitution.GetAllDomainTypes(m)) |> Set.unionMany
                | ast ->
                    Set.empty

    let rec replaceLast (f : string -> string) (id : list<Ident>) =
        match id with
            | [] -> []
            | [v] -> [Ident(f v.idText, v.idRange)]
            | h :: rest -> h :: replaceLast f rest

    
    let rec typeName (t : SynType) =
        match t with
            | SynType.LongIdent(LongIdentWithDots(id,_)) -> idString id |> Some
            | SynType.HashConstraint(t,_) -> typeName t
            | SynType.LongIdentApp(n, _, _, args, _, _, _) -> typeName n
            | SynType.WithGlobalConstraints(t,_,_) -> typeName t
            | _ -> None


    let tModRef = SynType.LongIdent(LongIdentWithDots([Ident("ModRef", range0)], []))
    let tMapSet = SynType.LongIdent(LongIdentWithDots([Ident("MapSet", range0)], []))
    let tResetSet = SynType.LongIdent(LongIdentWithDots([Ident("ResetSet", range0)], []))
    let tReuseCache = SynType.LongIdent(LongIdentWithDots([Ident("ReuseCache", range0)], []))
    let tId = SynType.LongIdent(LongIdentWithDots([Ident("Id", range0)], []))
    let tIUnique = SynType.LongIdent(LongIdentWithDots([Ident("IUnique", range0)], []))


    let rec toModTypeRef (domainTypes : Set<string>) (t : SynType) : SynType =
        match t with
            | SynType.LongIdent(LongIdentWithDots(id,r)) ->
                let n = idString id

                if Set.contains n domainTypes then
                    let newId = replaceLast (sprintf "M%s") id
                    SynType.LongIdent(LongIdentWithDots(newId, r))
                else
                    SynType.App(tModRef, None, [t], [], None, false, range0)

            | SynType.App(n, a, [imm], b, c, d, e) ->
                match typeName n with
                    | Some "pset" ->
                        match typeName imm with
                            | Some n when Set.contains n domainTypes ->
                                let mut = toModTypeRef domainTypes imm
                                SynType.App(tMapSet, a, [imm; mut], b, c, d, e)
                            | _ ->
                                SynType.App(tResetSet, a, [imm], b, c, d, e)
                    | _ ->
                        SynType.App(tModRef, None, [t], [], None, false, range0)

            | t ->
                SynType.App(tModRef, None, [t], [], None, false, range0)
                
                        

    let toModField (domainTypes : Set<string>) (f : SynField) : SynField =
        let (SynField.Field(attributes, isStatic, id, typeName, a, xmlDoc, accessibility, range)) = f

        match id with
            | Some id ->
                let newId = Ident("m" + id.idText, id.idRange)
                SynField.Field(attributes, isStatic, Some newId, toModTypeRef domainTypes typeName, a, xmlDoc, accessibility, range)
            | _ ->
                failwith ""

    let change (mExpr : SynExpr) (iExpr : SynExpr) (cache : SynExpr) (SynField.Field(id = mId; typeName = mType)) (SynField.Field(id = iId; typeName = iType)) =
        let mId = mId.Value
        let iId = iId.Value
        let mField = SynExpr.DotGet(mExpr, range0, LongIdentWithDots([mId], []), range0)
        let iField = SynExpr.DotGet(iExpr, range0, LongIdentWithDots([iId], []), range0)


        match mType with
            | MapSet(_) | ResetSet(_) -> 
                let arg = SynExpr.Tuple([iField], [], range0)
                SynExpr.App(ExprAtomicFlag.NonAtomic, false, SynExpr.DotGet(mField, range0, LongIdentWithDots([Ident("Update", range0)], []), range0), SynExpr.Paren(arg, range0, None, range0), range0)

            | ModRef(a) ->
                SynExpr.DotSet(mField, LongIdentWithDots([Ident("Value", range0)], []), iField, range0)

            | _ ->
                let arg = SynExpr.Tuple([iField], [], range0)
                SynExpr.App(ExprAtomicFlag.NonAtomic, false, SynExpr.DotGet(mField, range0, LongIdentWithDots([Ident("Apply", range0)], []), range0), SynExpr.Paren(SynExpr.Tuple([arg; cache], [range0], range0), range0, None, range0), range0)

         
    let init (iExpr : SynExpr) (cache : SynExpr) (SynField.Field(id = mId; typeName = mType)) (SynField.Field(id = iId; typeName = iType) as mf) =
        let mId = mId.Value
        let iId = iId.Value
        let iField = SynExpr.DotGet(iExpr, range0, LongIdentWithDots([iId], []), range0)

        let expression = 
            match mType with
                | MapSet(ivType, mvType) ->
                    // cache.GetCache()
                    let typedCache = SynExpr.App(ExprAtomicFlag.NonAtomic, false, SynExpr.DotGet(cache, range0, LongIdentWithDots([Ident("GetCache", range0)], []), range0), SynExpr.Const(SynConst.Unit, range0), range0)
    
                    let args = 
                        let m = SynExpr.Ident(Ident("m", range0))
                        let a = SynExpr.Ident(Ident("a", range0))
                        let create =
                            // fun (a : iType) -> a.ToMod(cache)
                            let body = SynExpr.App(ExprAtomicFlag.NonAtomic, false, SynExpr.DotGet(a, range0, LongIdentWithDots([Ident("ToMod", range0)], []), range0), SynExpr.Paren(cache, range0, None, range0), range0)
                            let a = SynSimplePat.Typed(SynSimplePat.Id(Ident("a", range0), None, false, false, false, range0), ivType, range0)
                            SynExpr.Lambda(false, false, SynSimplePats.SimplePats([a], range0), body, range0)

                        let update =
                            // fun (m : mType, a : iType) -> m.Apply(a, cache)
                            let body = SynExpr.App(ExprAtomicFlag.NonAtomic, false, SynExpr.DotGet(m, range0, LongIdentWithDots([Ident("Apply", range0)], []), range0), SynExpr.Paren(SynExpr.Tuple([a; cache], [range0], range0), range0, None, range0), range0)
                            let a = SynSimplePat.Typed(SynSimplePat.Id(Ident("a", range0), None, false, false, false, range0), ivType, range0)
                            let m = SynSimplePat.Typed(SynSimplePat.Id(Ident("m", range0), None, false, false, false, range0), mvType, range0)
                       
                            SynExpr.Lambda(false, false, SynSimplePats.SimplePats([m;a], range0), body, range0)


                        SynExpr.Tuple(
                            [
                                SynExpr.Paren(typedCache, range0, None, range0)
                                iField
                                SynExpr.Paren(create, range0, None, range0)
                                SynExpr.Paren(update, range0, None, range0)

                            ],
                            [ range0; range0 ],
                            range0
                        )

                    SynExpr.App(ExprAtomicFlag.NonAtomic, false, SynExpr.Ident(Ident("MapSet", range0)), SynExpr.Paren(args, range0, None, range0), range0)
            
                | ResetSet(_) -> 
                    let args = iField
                    SynExpr.App(ExprAtomicFlag.NonAtomic, false, SynExpr.Ident(Ident("ResetSet", range0)), SynExpr.Paren(args, range0, None, range0), range0)

                | ModRef(a) ->
                    let args = iField
                    SynExpr.App(ExprAtomicFlag.NonAtomic, false, SynExpr.LongIdent(false, LongIdentWithDots([Ident("Mod", range0); Ident("init", range0)], [range0]), None, range0), SynExpr.Paren(args, range0, None, range0), range0)

                | _ ->
                    let arg = SynExpr.Tuple([iField], [], range0)
                    SynExpr.App(ExprAtomicFlag.NonAtomic, false, SynExpr.DotGet(iField, range0, LongIdentWithDots([Ident("ToMod", range0)], []), range0), SynExpr.Paren(cache, range0, None, range0), range0)

        LongIdentWithDots([mId], []), expression

    let toModType (domainTypes : Set<string>) (t : SynTypeDefn) =
        let (SynTypeDefn.TypeDefn(oldCi,repr,mems,tdRange)) = t
        let (SynComponentInfo.ComponentInfo(att,typeParams,constraints,id,xmlDoc,preferPostfix,accessibility,range)) = oldCi
        let newId = id |> replaceLast (sprintf "M%s")
        let tName = SynType.LongIdent(LongIdentWithDots(id, []))
        let tmName = SynType.LongIdent(LongIdentWithDots(newId, []))

        let ci =
            SynComponentInfo.ComponentInfo(
                att,
                typeParams, constraints,
                newId,
                xmlDoc,
                preferPostfix,
                accessibility,
                range
            )

        match repr with
            | SynTypeDefnRepr.Simple(simple,_) ->
                match simple with
                    | SynTypeDefnSimpleRepr.Record(access, fields, range) -> 
                        let newFields = fields |> List.map (toModField domainTypes)
                        let originalField = SynField.Field([], false, Some (Ident("_original", range0)), tName, true, PreXmlDocEmpty, None, range0)
                        let idField = SynField.Field([], false, Some (Ident("_id", range0)), tId, true, PreXmlDocEmpty, None, range0)
                        let self = SynExpr.Ident(Ident("x", range0))
                        let reuseCache = SynExpr.Ident(Ident("reuseCache", range0))
                        let originalId = LongIdentWithDots([Ident("_original", range0)], [])

                            
                        let apply = 
                            let arg0 = SynExpr.Ident(Ident("arg0", range0))
                            let changes = List.map2 (change self arg0 reuseCache) newFields fields |> SynExpr.Seq
                            let equalId = LongIdentWithDots([Ident("System", range0); Ident("Object", range0); Ident("ReferenceEquals", range0)], [range0; range0])
                            let org = SynExpr.DotGet(self, range0, originalId, range0)

                            let guard = 
                                let args = SynExpr.Paren(SynExpr.Tuple([arg0; org], [range0], range0), range0, None, range0)
                                let neg = SynExpr.App(ExprAtomicFlag.NonAtomic, false, SynExpr.LongIdent(false, equalId, None, range0), args, range0)
                                SynExpr.App(ExprAtomicFlag.NonAtomic, false, SynExpr.Ident(Ident("not", range0)), SynExpr.Paren(neg, range0, None, range0), range0)

                            let guardedChanges = 
                                SynExpr.IfThenElse(
                                    guard,
                                    SynExpr.Seq [
                                        SynExpr.DotSet(self, originalId, arg0, range0)
                                        changes
                                    ],
                                    None,
                                    SequencePointInfoForBinding.NoSequencePointAtDoBinding,
                                    false, 
                                    range0,
                                    range0
                                )

                            let svData =
                                let flags =
                                    {
                                        IsInstance = true
                                        IsDispatchSlot = false
                                        IsFinal = false
                                        IsOverrideOrExplicitImpl = false
                                        MemberKind = MemberKind.Member
                                    }

                                SynValData(
                                    Some flags, 
                                    SynValInfo(
                                        [
                                            [SynArgInfo.SynArgInfo([], false, Some (Ident("x", range0)))]
                                            [SynArgInfo.SynArgInfo([], false, Some (Ident("arg0", range0))); SynArgInfo.SynArgInfo([], false, Some (Ident("reuseCache", range0)))]
                                        ], 
                                        SynArgInfo([], false, None)), 
                                        Some (Ident("Apply", range0)
                                    )
                                )

                            let headPat =
                                let args =
                                    SynConstructorArgs.Pats [
                                        SynPat.Paren(
                                            SynPat.Tuple(
                                                [
                                                    SynPat.Typed(
                                                        SynPat.Named(SynPat.Wild range0, (Ident("arg0", range0)), false, None, range0), 
                                                        tName,
                                                        range0
                                                    )
                                                    SynPat.Typed(
                                                        SynPat.Named(SynPat.Wild range0, (Ident("reuseCache", range0)), false, None, range0), 
                                                        tReuseCache,
                                                        range0
                                                    )
                                                ], 
                                                range0
                                            ),
                                            range0
                                        )
                                    ]
                                let id = LongIdentWithDots([Ident("x", range0); Ident("Apply", range0)], [range0])
                                SynPat.LongIdent(id, None, None, args, None, range0)

                            let binding =
                                SynBinding.Binding(
                                    None, 
                                    SynBindingKind.NormalBinding, 
                                    false, false, 
                                    [], PreXmlDocEmpty, 
                                    svData, 
                                    headPat, 
                                    None, 
                                    guardedChanges, 
                                    range0, SequencePointInfoForBinding.NoSequencePointAtLetBinding
                                )

                            SynMemberDefn.Member(binding, range0)

                        let toMod =
                            let svData =
                                let flags =
                                    {
                                        IsInstance = true
                                        IsDispatchSlot = false
                                        IsFinal = false
                                        IsOverrideOrExplicitImpl = false
                                        MemberKind = MemberKind.Member
                                    }

                                SynValData(
                                    Some flags, 
                                    SynValInfo(
                                        [
                                            [SynArgInfo.SynArgInfo([], false, Some (Ident("x", range0)))]
                                            [SynArgInfo.SynArgInfo([], false, Some (Ident("reuseCache", range0)))]
                                        ], 
                                        SynArgInfo([], false, None)
                                    ), 
                                    Some (Ident("ToMod", range0))
                                )

                            let headPat =
                                let args =
                                    SynConstructorArgs.Pats [
                                        SynPat.Paren(
                                            SynPat.Typed(
                                                SynPat.Named(SynPat.Wild range0, (Ident("reuseCache", range0)), false, None, range0), 
                                                tReuseCache,
                                                range0
                                            ),
                                            range0
                                        )
                                    ]
                                let id = LongIdentWithDots([Ident("x", range0); Ident("ToMod", range0)], [range0])
                                SynPat.LongIdent(id, None, None, args, None, range0)


                            let inits = 
                                let real = List.map2 (init self reuseCache) newFields fields
                                (originalId, self) :: real
                            let init =
                                let args = 
                                    inits |> List.map (fun (f,v) ->
                                        let name = (f, true)
                                        (name, Some v, Some (range0, None))
                                    )
                                SynExpr.Record(None, None, args, range0)

                            let binding =
                                SynBinding.Binding(
                                    None, 
                                    SynBindingKind.NormalBinding, 
                                    false, false, 
                                    [], PreXmlDocEmpty, 
                                    svData, 
                                    headPat, 
                                    Some (SynBindingReturnInfo.SynBindingReturnInfo(tmName, range0, [])), 
                                    init, 
                                    range0, 
                                    SequencePointInfoForBinding.NoSequencePointAtLetBinding
                                )
                                
                            SynMemberDefn.Member(binding, range0)

                        let iUniqueImpl =
                            let getter = 
                                let svData =
                                    let flags =
                                        {
                                            IsInstance = true
                                            IsDispatchSlot = false
                                            IsFinal = false
                                            IsOverrideOrExplicitImpl = false
                                            MemberKind = MemberKind.PropertyGet
                                        }

                                    SynValData(
                                        Some flags, 
                                        SynValInfo(
                                            [
                                                [SynArgInfo.SynArgInfo([], false, Some (Ident("x", range0)))]
                                                [SynArgInfo.SynArgInfo([], false, None)]
                                            ], 
                                            SynArgInfo([], false, None)
                                        ), 
                                        Some (Ident("Id", range0))
                                    )

                                let headPat =
                                    let args = SynConstructorArgs.Pats [SynPat.Const(SynConst.Unit, range0)]
                                    let id = LongIdentWithDots([Ident("x", range0); Ident("Id", range0)], [range0])
                                    SynPat.LongIdent(id, None, None, args, None, range0)

                                let expr = SynExpr.DotGet(self, range0, LongIdentWithDots([Ident("_id", range0)], []), range0)

                                let binding =
                                    SynBinding.Binding(
                                        None, 
                                        SynBindingKind.NormalBinding, 
                                        false, false, 
                                        [], PreXmlDocEmpty, 
                                        svData, 
                                        headPat, 
                                        None, 
                                        expr, 
                                        range0, 
                                        SequencePointInfoForBinding.NoSequencePointAtLetBinding
                                    )

                                SynMemberDefn.Member(binding, range0)

                            let setter = 
                                let svData =
                                    let flags =
                                        {
                                            IsInstance = true
                                            IsDispatchSlot = false
                                            IsFinal = false
                                            IsOverrideOrExplicitImpl = false
                                            MemberKind = MemberKind.PropertySet
                                        }

                                    SynValData(
                                        Some flags, 
                                        SynValInfo(
                                            [
                                                [SynArgInfo.SynArgInfo([], false, Some (Ident("x", range0)))]
                                                [SynArgInfo.SynArgInfo([], false, Some (Ident("id", range0)))]
                                            ], 
                                            SynArgInfo([], false, None)
                                        ), 
                                        Some (Ident("Id", range0))
                                    )

                                let headPat =
                                    let args = SynConstructorArgs.Pats [SynPat.Named(SynPat.Wild range0, (Ident("v", range0)), false, None, range0)]
                                    let id = LongIdentWithDots([Ident("x", range0); Ident("Id", range0)], [range0])
                                    SynPat.LongIdent(id, None, None, args, None, range0)

                                let expr = SynExpr.DotSet(self, LongIdentWithDots([Ident("_id", range0)], []), SynExpr.Ident(Ident("v", range0)), range0)

                                let binding =
                                    SynBinding.Binding(
                                        None, 
                                        SynBindingKind.NormalBinding, 
                                        false, false, 
                                        [], PreXmlDocEmpty, 
                                        svData, 
                                        headPat, 
                                        None, 
                                        expr, 
                                        range0, 
                                        SequencePointInfoForBinding.NoSequencePointAtLetBinding
                                    )

                                SynMemberDefn.Member(binding, range0)
                            
                            SynMemberDefn.Interface(
                                tIUnique, 
                                Some [getter; setter],
                                range0
                            )

                        let modRepr = SynTypeDefnRepr.Simple(SynTypeDefnSimpleRepr.Record(access, originalField :: newFields, range), range)
                        let newRepr = SynTypeDefnRepr.Simple(SynTypeDefnSimpleRepr.Record(access, idField :: fields, range), range)

                        let modType = SynTypeDefn.TypeDefn(ci, modRepr, [apply], tdRange)
                        let newType = SynTypeDefn.TypeDefn(oldCi, newRepr, mems @ [toMod; iUniqueImpl], tdRange)

                        newType, modType

                    | _ ->
                        failwith "unknown kind"
            | _ ->
                failwith "unknown kind"


    [<CompiledName("Run")>]
    let run (log : TaskLoggingHelper) (file : string) (outputFile : string) =
        let run = 
            async {
                let input = File.ReadAllText file
                let! projOptions = checker.GetProjectOptionsFromScript(file, input)
                let! results = checker.ParseFileInProject(file, input, projOptions)

                match results.ParseTree with
                    | Some ast ->
//                        try
                            let nast =
                                let domainTypes = ast.GetAllDomainTypes() |> Set.map snd
                                ast.SubstituteTypeDef(fun ns types ->
                                    types |> List.collect (fun t ->
                                        if t.IsDomainType then
                                            let (t', tm) = toModType domainTypes t
                                            [t'; tm]
                                        else
                                            [t]
                                    )
                                )

                            let str = Fantomas.CodeFormatter.FormatAST(nast, file, None, { Fantomas.FormatConfig.FormatConfig.Default with StrictMode = true })
                            File.WriteAllText(outputFile, str)
                            return true
//
//                            let domainTypes = findDomainTypes log ast
//                            match domainTypes with
//                                | [] -> 
//                                    File.WriteAllText(outputFile, "namespace EmptyDomainTypesNamespace")
//                                    log.LogWarning "no domain-types found"
//                                    return false
//                                | _ -> 
//                                    let str = Generator.generate domainTypes
//                                    File.WriteAllText(outputFile, str)
//                                    return true
//                        with e ->
//                           
//                            return false

                    | None ->
                        File.Delete outputFile
                        return false
            }
        Async.RunSynchronously run

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
            let res = Preprocessing.run x.Log (Path.Combine(System.Environment.CurrentDirectory, item)) (Path.Combine(System.Environment.CurrentDirectory,domFile)) 
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