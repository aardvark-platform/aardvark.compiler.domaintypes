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

type Remote(path : string) =

    //let resolve =
    //    ResolveEventHandler(fun (s : obj) (e : ResolveEventArgs) ->
    //        let n = System.Reflection.AssemblyName(e.Name)
    //        if n.Name = "FSharp.Core" then
    //            System.Reflection.Assembly.LoadFile @"C:\Users\Schorsch\Development\diffgenerator\lib\net45\FSharp.Core.dll"
    //        else
    //            let testFile = Path.Combine(path, n.Name + ".dll")
    //            if File.Exists testFile then
    //                try
    //                    System.Reflection.Assembly.LoadFile testFile
    //                with _ ->
    //                    null
    //            else
    //                null
    //    )

    //do AppDomain.CurrentDomain.add_AssemblyResolve(resolve)

    member x.Call(projectFile : string, references : string[], files : string[]) =
        FSharpChecker.Create(200, true, true) |> ignore



type Preprocess() =
    inherit Task()

    let mutable debug = false
    let mutable files : string[] = [||]
    let mutable references : string[] = [||]
    let mutable projectFile = ""
    let mutable results : string[] = [||]


    override x.Execute() =
        let path = Path.GetDirectoryName(typeof<Preprocess>.Assembly.Location)
        if System.Runtime.InteropServices.RuntimeInformation.FrameworkDescription.ToLower().Contains "core" then
            Remote(path).Call(projectFile, references, files)
        else
            let tSetup = typeof<System.Reflection.Assembly>.Assembly.GetType("System.AppDomainSetup")
            let cSetup = tSetup.GetConstructor([||])
            let pAppBase = tSetup.GetProperty("ApplicationBase")

            let instance = cSetup.Invoke([||])
            pAppBase.SetValue(instance, path)

            let tEvidence = typeof<System.Reflection.Assembly>.Assembly.GetType("System.Security.Policy.Evidence")
            let creator = typeof<AppDomain>.GetMethod("CreateDomain", [| typeof<string>; tEvidence; tSetup|])
            let domain = creator.Invoke(null, [|"TempDomain" :> obj; null; instance|]) |> unbox<AppDomain>

            
            
            System.Diagnostics.Debugger.Launch() |> ignore
            let self = domain.Load "Aardvark.Compiler.DomainTypes.MSBuild"
            let t = self.GetType(typeof<Remote>.FullName)
            let temp = Activator.CreateInstance(t, [| path :> obj |]) |> unbox<Remote>
            temp.Call(projectFile, references, files)

            AppDomain.Unload(domain)



        //x.Log.LogWarning(System.Runtime.InteropServices.RuntimeInformation.FrameworkDescription)
        
        //Environment.CurrentDirectory <- Path.GetDirectoryName(typeof<Preprocess>.Assembly.Location)
        
        
        //Remote().Call(projectFile, references, files)
        

        //System.Diagnostics.Debugger.Launch() |> ignore
        //FSharpChecker.Create(200, true, true) |> ignore

        //let d = AppDomain.CreateDomain("TempDomain")
        //let remote = d.CreateInstanceAndUnwrap("Aardvark.Compiler.DomainTypes.MSBuild", "Aardvark.Compiler.DomainTypes.Remote") |> unbox<Remote>
        //let prep = remote.Call(projectFile, references, files)
        //AppDomain.Unload(d)



        //for r in references do
        //    x.Log.LogWarning(sprintf "reference: %A" r)
        results <- files
        true

        //if debug then
        //    System.Diagnostics.Debugger.Launch() |> ignore


        ////let prep = Preprocessing.run projectFile (Set.ofArray references) (Array.toList files) |> Async.RunSynchronously
        //results <- files

        //let projectDir = Path.GetDirectoryName projectFile

        //match prep with
        //    | Worked res ->
        //        let mutable goodFiles = Set.empty
        //        for (f,content) in Map.toSeq res do
        //            match content with
        //                | Finished(warnings, content) ->
        //                    for w in warnings do 
        //                        x.Log.LogWarning(w)

        //                    goodFiles <- Set.add f goodFiles

        //                    let path = System.IO.Path.ChangeExtension(f, ".g.fs")
        //                    x.Log.LogMessage(sprintf "generated DomainFile %s" (Path.GetFileName path))

        //                    let old = 
        //                        if File.Exists path then File.ReadAllText path
        //                        else ""

        //                    if old <> content then
        //                        File.WriteAllText(path, content)
        //                | Faulted(warnings, error) ->
        //                    for w in warnings do x.Log.LogWarning(w)
        //                    x.Log.LogError(error)

        //        let files = 
        //            files |> Array.map (fun f ->
        //                Path.Combine(projectDir, f) |> Path.GetFullPath
        //            )

        //        let fscFiles =
        //            files |> Array.collect (fun f ->
        //                let gf = System.IO.Path.ChangeExtension(f, ".g.fs")

        //                if files |> Array.exists (fun f -> f = gf) then
        //                    [| f |]
        //                else
        //                    if Set.contains f goodFiles then
        //                        [| f; gf |]
        //                    else
        //                        [| f |]
        //            )

        //        results <- fscFiles

        //        true

        //    | CompilerError errors ->
        //        x.Log.LogError("F# compiler returned errors")
        //        false


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