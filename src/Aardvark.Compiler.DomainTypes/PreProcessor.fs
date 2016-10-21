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

                | Set rt -> 
                    let inner = realTypeString m rt
                    match m with
                        | Immutable -> 
                            return sprintf "pset<%s>" inner

                        | Mutable -> 
                            let iInner = realTypeString Immutable rt
                            return sprintf "MapSet<%s, %s>" iInner inner

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
                            | Immutable -> do! Generator.line "mutable _id : int64"
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

                                do! Generator.line "_id = 0L"
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
                        do! Generator.line "static member ToMod(l : %s) = " iName
                        do! Generator.push

                        do! Generator.line "{"
                        do! Generator.push

                        do! Generator.line "original = l"

                        for (n,t) in d.fields do
                            match t with
                                | Value t ->
                                    match t with
                                        | Local _ ->
                                            do! Generator.line "m%s = Differential.ToMod(l.%s)" n n
                                        | _ ->
                                            do! Generator.line "m%s = Mod.init l.%s" n n
                                | Set t ->
                                    match t with
                                        | Local _ ->
                                            do! Generator.line "m%s = MapSet(l.%s, Differential.ToMod, Differential.Apply)" n n
                                        | _ ->
                                            do! Generator.line "m%s = CSet.ofSeq l.%s" n n
                                    
                            
                                    
                            ()

                        do! Generator.pop
                        do! Generator.line "}"

                        do! Generator.pop
                        do! Generator.line ""

                    | Union d ->
                        let mName = realTypeString Mutable (Local d.name)
                        let iName = realTypeString Immutable (Local d.name)
                        do! Generator.line "static member ToMod(l : %s) : %s = " iName mName
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
                        do! Generator.line "static member Apply(r : %s, l : %s) = " mName iName
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
                                            do! Generator.line "CSet.change r.m%s l.%s" n n 
                    
                            ()

                        do! Generator.pop
                        do! Generator.pop
                        do! Generator.line ""

                    | Union d ->
                        let mName = realTypeString Mutable (Local d.name)
                        let iName = realTypeString Immutable (Local d.name)

                        do! Generator.line "static member Apply(r : %s, l : %s) : unit = " mName iName
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
    let checker = FSharpChecker.Create()




    let lines =
        [
            "namespace Blubber"
            "type Seppy() ="
            "    inherit Test.BaseType()"
            "    member x.A = 10"
        ]

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


    let toAst (locals : Set<string>) (name : string) (def : SynTypeDefnSimpleRepr) =
        match def with
            | SynTypeDefnSimpleRepr.Record(_,fields,_) ->
                let fields =
                    fields |> List.map (fun (SynField.Field(_,_,id,t,_,_,_,_)) ->
                        let mini = toMiniType t

                        id.Value.idText, toTypeRef locals mini
                    )

                Record { name = name; fields = fields }
            | _ ->
                failwith ""

    let rec findDomTypes (ns : string) (opened : ref<list<string>>) (locals : Dictionary<string, HashSet<string>>) (d : SynModuleDecl) =
        match d with
            | SynModuleDecl.NestedModule(comp,_,decls,_,_) ->
                let (SynComponentInfo.ComponentInfo(attributes, typeParams, _, typeId, _, _, _, _)) = comp
                let name = idString typeId
                let ns = ns + "." + name
                let inner = ref !opened
                decls |> List.collect (fun d -> findDomTypes ns inner locals d)

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
                                [ { ns = ns; opens = List.rev !opened; def = toAst visibleLocals name simple }]
                                //[!opened, ns, name, toAst visibleLocals name simple]
                            | _ ->
                                failwith ""
                    else
                        []
                )

            | _ -> []

    let findDomainTypes (ast : ParsedInput) =
        match ast with
            | ParsedInput.ImplFile(ParsedImplFileInput(_, _, _, _, _, modulesAndNamespaces,_)) ->
                let locals = Dictionary()
                modulesAndNamespaces |> List.collect (fun (SynModuleOrNamespace(id,_,_,decls,_,_,_,_)) ->
                    let ns = idString id
                    let opened = ref []

                    decls |> List.collect (fun d ->
                        findDomTypes ns opened locals d
                    )
                )
            | _ ->
                []

    [<CompiledName("Run")>]
    let run (file : string) (outputFile : string) =
        let run = 
            async {
                let input = File.ReadAllText file
                let! projOptions = checker.GetProjectOptionsFromScript(file, input)
                let! results = checker.ParseFileInProject(file, input, projOptions)

                match results.ParseTree with
                    | Some ast ->
                        let domainTypes = findDomainTypes ast

                        let str = Generator.generate domainTypes

        
                        File.WriteAllText(outputFile, str)

                    | None ->
                        File.Delete outputFile
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
            Preprocessing.run (Path.Combine(System.Environment.CurrentDirectory, item)) (Path.Combine(System.Environment.CurrentDirectory,domFile))

            results <- [|item; domFile|]
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