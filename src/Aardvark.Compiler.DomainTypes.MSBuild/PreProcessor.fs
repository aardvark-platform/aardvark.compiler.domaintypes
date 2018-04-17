namespace Aardvark.Compiler.DomainTypes

open System
open System.Reflection
open System.IO
open CodeGen
open Microsoft.Build.Framework
open Microsoft.Build.Utilities
open Microsoft.FSharp.Compiler
open Microsoft.FSharp.Compiler.SourceCodeServices
    
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
        member x.LogError(w : FSharpErrorInfo) =
            x.LogError(
                "", 
                string w.ErrorNumber, 
                "", 
                w.FileName, 
                w.StartLineAlternate, w.StartColumn + 1, 
                w.EndLineAlternate, w.EndColumn + 1, 
                w.Message,
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

                for e in errors do
                    x.Log.LogError(e)

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