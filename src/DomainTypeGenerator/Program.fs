// Learn more about F# at http://fsharp.org

open System
open System.IO
open Aardvark.Base
open Aardvark.Compiler.DomainTypes

open Dotnet.ProjInfo
open Dotnet.ProjInfo.Workspace

let rec private projInfo additionalMSBuildProps file =

    let projDir = Path.GetDirectoryName file
    let runCmd exePath args = Utils.runProcess ignore projDir exePath (args |> String.concat " ")
    
    let additionalMSBuildProps = ("GenerateDomainTypes", "false") :: additionalMSBuildProps

    let netcore =
        match file with
        | ProjectRecognizer.NetCoreSdk -> true
        | _ -> false
    
    let projectAssetsJsonPath = Path.Combine(projDir, "obj", "project.assets.json")
    if netcore && not(File.Exists(projectAssetsJsonPath)) then
        let (s, a) = runCmd "dotnet" ["restore"; sprintf "\"%s\"" file]
        if s <> 0 then 
            failwithf "Cannot find restored info for project %s" file
    
    let getFscArgs = 
        if netcore then
            Dotnet.ProjInfo.Inspect.getFscArgs
        else
            let asFscArgs props =
                let fsc = Microsoft.FSharp.Build.Fsc()
                Dotnet.ProjInfo.FakeMsbuildTasks.getResponseFileFromTask props fsc
            Dotnet.ProjInfo.Inspect.getFscArgsOldSdk (asFscArgs >> Ok)

    let results =
        let msbuildExec =
            let msbuildPath =
                if netcore then Dotnet.ProjInfo.Inspect.MSBuildExePath.DotnetMsbuild "dotnet"
                else 
                    let all = 
                        BlackFox.VsWhere.VsInstances.getWithPackage "Microsoft.Component.MSBuild" true

                    let probes =
                        [
                            @"MSBuild\Current\Bin\MSBuild.exe"
                            @"MSBuild\15.0\Bin\MSBuild.exe"
                        ]

                    let msbuild =
                        all |> List.tryPick (fun i ->
                            probes |> List.tryPick (fun p ->
                                let path = Path.Combine(i.InstallationPath, p)
                                if File.Exists path then Some path
                                else None
                            )
                        )

                    match msbuild with
                    | Some msbuild -> Dotnet.ProjInfo.Inspect.MSBuildExePath.Path msbuild
                    | None ->
                        failwith "no msbuild"
            Dotnet.ProjInfo.Inspect.msbuild msbuildPath runCmd

        let additionalArgs = additionalMSBuildProps |> List.map (Dotnet.ProjInfo.Inspect.MSBuild.MSbuildCli.Property)

        let log = ignore


        file
        |> Inspect.getProjectInfos log msbuildExec [getFscArgs] additionalArgs

    netcore, results

[<EntryPoint>]
let main argv =
    if argv.Length < 1 then
        Log.error "usage [paths-to-fsproj(s)]"
        -2
    else
        let mutable status = 0
        for file in argv do
            let argv = ()
            let file = Path.GetFullPath file
            if File.Exists file then 
                let sw = System.Diagnostics.Stopwatch.StartNew()
                Log.start "%s" (Path.GetFileName file)
                let netcore, res = projInfo [] file
                match res with
                | Result.Ok [Ok (Inspect.GetResult.FscArgs a)] ->
                    let messages = System.Collections.Generic.List<ErrorInfo>()
                    let log (e : ErrorInfo) = messages.Add e

                    let references = a |> List.choose (fun o -> if o.StartsWith "-r:" then Some (o.Substring 3) else None) |> Set.ofList
                    let files = 
                        a 
                        |> List.choose (fun o -> if not (o.StartsWith "-") then Some o else None)
                        |> List.filter (fun f -> not (Path.GetFileName(f).ToLower().Trim().EndsWith "assemblyinfo.fs"))
                        |> List.filter (fun f -> not (Path.GetFileName(f).ToLower().Trim().EndsWith "assemblyattributes.fs"))

                    let targetType =
                        a 
                        |> List.tryPick (fun o ->
                            if o.StartsWith "--target:" then
                                let t = o.Substring(9).ToLower().Trim()
                                match t with
                                | "exe" -> Some TargetType.Exe
                                | "winexe" -> Some TargetType.WinExe
                                | _ -> Some TargetType.Library
                            else
                                None
                        )
                        |> Option.defaultValue TargetType.Library

                    let res = 
                        Preprocessing.runFileByFile 
                            (not netcore)
                            log
                            file
                            targetType
                            references
                            files
                        |> Async.RunSynchronously
                    
                    sw.Stop()

                    let messages = messages |> Seq.filter (fun m -> m.severity = Severity.Warning || m.severity = Severity.Error) |> Seq.toArray

                    if messages.Length > 0 then 
                        let errors = messages |> Array.exists (fun m -> m.severity = Severity.Error)
                        Log.start "generate returned %s" (if errors then "errors" else "warnings")
                        for msg in messages do
                            let name = Path.GetFileName msg.file
                            match msg.severity with
                            | Severity.Error -> Log.error "%s(%d,%d): %s" name msg.startLine msg.startColumn msg.message
                            | Severity.Warning -> Log.warn "%s(%d,%d): %s" name msg.startLine msg.startColumn msg.message
                            | _ -> () 
                        Log.stop()

                    match res with
                    | Some generated -> 
                        let gen = 
                            generated 
                            |> Array.filter (fun f -> Path.GetFileName(f).EndsWith ".g.fs") 
                            |> Array.map Path.GetFileName
                            
                        if gen.Length > 0 then
                            Log.start "generate took %A" sw.MicroTime
                            for g in gen do Log.line "%s" g
                            Log.stop()
                        else
                            Log.line "nothing generated"
                        
                    | None ->
                        Log.error "failed"
                        status <- -1
                
                | Result.Ok _ -> 
                    Log.error "failed"
                    status <- -1

                | Result.Error err ->
                    match err with
                    | Inspect.GetProjectInfoErrors.MSBuildFailed(code,(_,exe,cmd)) ->
                        Log.error "msbuild failed: %d" code
                        Log.error "%s %s" exe cmd
                    | Inspect.GetProjectInfoErrors.UnexpectedMSBuildResult(str) ->
                        Log.error "msbuild failed: %s" str
                    | Inspect.GetProjectInfoErrors.MSBuildSkippedTarget ->
                        Log.error "msbuild failed"
                        
                    status <- -1


                Log.stop()
            else
                status <- -2
        status
