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
        let proj = argv.[0]
        printf "cracking project ..."
        let options = ProjectCracker.GetProjectOptionsFromProjectFile proj
        printfn " done"

        printf "processing ..."
        let res = Preprocessing.runWithOptions options |> Async.RunSynchronously
        match res with
            | Some res ->
                printfn " done"
                printfn "writing output"
                for (f,content) in Map.toSeq res do
                    let path = System.IO.Path.ChangeExtension(f, ".g.fs")
                    printf "    %s" (Path.GetFileName path)
                    let old = 
                        if File.Exists path then File.ReadAllText path
                        else ""

                    if old <> content then
                        File.WriteAllText(path, content)

                    printfn ""
                printfn "done"
                0
            | None ->
                printfn "error"
                -1
