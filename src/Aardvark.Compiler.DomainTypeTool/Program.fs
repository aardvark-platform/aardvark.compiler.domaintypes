open System
open System.IO
open System.Diagnostics
open Microsoft.FSharp.Compiler
open Microsoft.FSharp.Compiler.SourceCodeServices.ProjectCrackerTool
open Microsoft.FSharp.Compiler.SourceCodeServices
open Aardvark.Compiler.DomainTypes

[<EntryPoint>]
let main argv = 
    if argv.Length < 1 then
        let me = Process.GetCurrentProcess().ProcessName    
        printfn "usage: %s <fsprojpath>" me
        -1
    else
        let sw = System.Diagnostics.Stopwatch()
        let proj = argv.[0]
        printf "cracking project: "
        sw.Start()
        let options = ProjectCracker.GetProjectOptionsFromProjectFile proj
        sw.Stop()
        printfn "done (%.0fs)" sw.Elapsed.TotalSeconds

        printf "processing: "
        sw.Restart()
        let res = Preprocessing.runWithOptions options |> Async.RunSynchronously
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
