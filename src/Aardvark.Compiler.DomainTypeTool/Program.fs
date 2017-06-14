open System
open System.IO
open System.Diagnostics
open Microsoft.FSharp.Compiler
open Microsoft.FSharp.Compiler.SourceCodeServices
open Aardvark.Compiler.DomainTypes


open Microsoft.Build
open Microsoft.Build.BuildEngine
open Microsoft.Build.Utilities
open Microsoft.Build.Evaluation
open Microsoft.Build.Framework

type FSharpProject =
    {
        path : string
        files : list<string>
        references : Set<string>
    }

let crackProject (file : string) =
    try
        use engine = new ProjectCollection()
    
        engine.SetGlobalProperty("VisualStudioVersion", "15.0") |> ignore
        engine.SetGlobalProperty("Configuration", "Debug") |> ignore
        engine.SetGlobalProperty("Platform", "AnyCPU") |> ignore

        let project = engine.LoadProject(file)
        let instance = project.CreateProjectInstance()
        instance.Build([|"ResolveReferences"|], []) |> ignore
    
        let dir = Path.GetDirectoryName(file)

        let references = 
            instance.GetItems("_ResolveAssemblyReferenceResolvedFiles") 
                |> Seq.map (fun i -> i.EvaluatedInclude) 
                |> Set.ofSeq

        let files = 
            instance.GetItems("Compile") 
                |> Seq.map (fun i -> i.EvaluatedInclude) 
                |> Seq.map (fun p -> Path.Combine(dir, p) |> Path.GetFullPath)
                |> Seq.toList
                
        Some {
            path = file
            files = files
            references = references
        }
    with e ->
        printfn "[cracking project] with message: %s at:\n\n%A" e.Message e.StackTrace
        None

let doIt proj =
    let sw = System.Diagnostics.Stopwatch()
    printf "cracking project: "
    sw.Start()
    let cracked = crackProject proj
    sw.Stop()
    match cracked with
        | Some project ->
            printfn "done (%.0fs)" sw.Elapsed.TotalSeconds

            printf "processing: "
            sw.Restart()
            let res = Preprocessing.run project.path project.references project.files |> Async.RunSynchronously
            match res with
                | Worked res ->
                    sw.Stop()
                    printfn "done (%.0fs)" sw.Elapsed.TotalSeconds
                    printfn "writing output"
                    for (f,prep) in Map.toSeq res do
                        match prep with
                            | Finished(warnings, content) ->
                                let path = System.IO.Path.ChangeExtension(f, ".g.fs")
                                printf "    %s: " (Path.GetFileName f)

                                for w in warnings do 
                                    printfn ""
                                    printf "        WARNING: %A" w

                                let old = 
                                    if File.Exists path then File.ReadAllText path
                                    else ""

                                if old <> content then
                                    File.WriteAllText(path, content)

                                if List.isEmpty warnings then
                                    printfn "done"
                                else
                                    printfn ""

                            | Faulted(warnings, err) ->
                                printfn "    %s: " (Path.GetFileName f)
                                for w in warnings do 
                                    printfn "        WARNING: %A" w

                                printfn "        ERROR: %A" err
                    0
                | CompilerError errors ->
                    sw.Stop()
                    printfn "faulted"
                    for e in errors do
                        printfn "    ERROR: %A" e
                    -1
        | None ->
            printfn "faulted"
            -1


[<EntryPoint>]
let main argv = 
    if argv.Length < 1 then
        let me = Process.GetCurrentProcess().ProcessName    
        printfn "usage: %s <fsprojpath>" me
        -1
    else
        let proj = argv.[0]

        if File.Exists proj then 
            doIt proj
        else 
            printfn "could not read project file. Make sure that the path exists and points to a .fsproj file."
            -1