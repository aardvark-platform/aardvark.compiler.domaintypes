// Learn more about F# at http://fsharp.org

open System
open System.IO
open System.Text.RegularExpressions
open Aardvark.Base
open Aardvark.Compiler.DomainTypes
open System.Runtime.InteropServices
open FSharp.Compiler.SourceCodeServices

[<Struct; StructuredFormatDisplay("{AsString}"); CustomEquality; CustomComparison>]
type SemVer(major : int, minor : int, build : int, revision : int, suffix : string) =
    static let rx = Regex @"^([0-9]+)\.([0-9]+)(\.[0-9]+)?(\.[0-9]+)?(-.*)?$"
    
    member x.Major = major
    member x.Minor = minor
    member x.Build = if build < 0 then 0 else build
    member x.Revision = if revision < 0 then 0 else revision
    member x.Suffix = if String.IsNullOrEmpty suffix then "" else suffix

    member private x.AsString = x.ToString()

    override x.ToString() =
        if String.IsNullOrWhiteSpace suffix then
            if revision >= 0 then String.Format("{0}.{1}.{2}.{3}", major, minor, build, revision)
            elif build >= 0 then String.Format("{0}.{1}.{2}", major, minor, build)
            else String.Format("{0}.{1}", major, minor)
        else
            if revision >= 0 then String.Format("{0}.{1}.{2}.{3}-{4}", major, minor, build, revision, suffix)
            elif build >= 0 then String.Format("{0}.{1}.{2}-{3}", major, minor, build, suffix)
            else String.Format("{0}.{1}-{2}", major, minor, suffix)
         
    static member TryParse(str : string, [<Out>] value : byref<SemVer>) =
        let m = rx.Match str
        if m.Success then
            let major = m.Groups.[1].Value |> int
            let minor = m.Groups.[2].Value |> int
            let build = if m.Groups.[3].Success then m.Groups.[3].Value.Substring(1) |> int else -1
            let revision = if m.Groups.[4].Success then m.Groups.[4].Value.Substring(1) |> int else -1
            let suffix = if m.Groups.[5].Success then m.Groups.[5].Value.Substring(1) else ""
            value <- SemVer(major, minor, build, revision, suffix)
            true
        else
            false
            
    member x.CompareTo (o : SemVer) =
        let c = compare x.Major o.Major
        if c <> 0 then c
        else
            let c = compare x.Minor o.Minor
            if c <> 0 then c
            else
                let c = compare x.Build o.Build
                if c <> 0 then c
                else
                    let c = compare x.Revision o.Revision
                    if c <> 0 then c
                    else
                        let xs = x.Suffix
                        let os = o.Suffix
                        if xs = "" && os = "" then 0
                        elif xs = "" then 1
                        elif os = "" then -1
                        else compare xs os

    override x.GetHashCode() =
        HashCode.Combine(x.Major.GetHashCode(), x.Minor.GetHashCode(), x.Build.GetHashCode(), x.Revision.GetHashCode(), x.Suffix.GetHashCode())

    override x.Equals (o : obj) =
        match o with
        | :? SemVer as o ->
            x.Major = o.Major &&
            x.Minor = o.Minor &&
            x.Build = o.Build &&
            x.Revision = o.Revision &&
            x.Suffix = o.Suffix
        | _ ->
            false

    interface IComparable with
        member x.CompareTo (o : obj) =
            match o with
            | :? SemVer as o -> x.CompareTo o
            | _ -> failwith "uncomparable"

    new(major : int, minor : int, build : int, revision : int) = SemVer(major, minor, build, revision, "")
    new(major : int, minor : int, build : int) = SemVer(major, minor, build, -1, "")
    new(major : int, minor : int) = SemVer(major, minor, -1, -1, "")

    new(major : int, minor : int, build : int, suffix : string) = SemVer(major, minor, build, -1, suffix)
    new(major : int, minor : int, suffix : string) = SemVer(major, minor, -1, -1, suffix)



type TargetFrameworkKind =
    | NetCoreApp
    | NetStandard
    | NetFramework

[<StructuredFormatDisplay("{AsString}")>]
type TargetFramework = { kind : TargetFrameworkKind; version : SemVer } with
    
    member private x.AsString = x.ToString()
    
    override x.ToString() =
        let v = x.version
        match x.kind with
        | NetCoreApp -> sprintf "netcoreapp%s" (string v)
        | NetStandard -> sprintf "netstandard%s" (string v)
        | NetFramework -> 
            let vstr =
                if v.Revision > 0 then sprintf "%d%d%d%d" v.Major v.Minor v.Build v.Revision
                elif v.Build > 0 then sprintf "%d%d%d" v.Major v.Minor v.Build
                else sprintf "%d%d" v.Major v.Minor
            sprintf "net%s" vstr


module TargetFramework =

    let private rx = Regex(@"^(netstandard|netcoreapp|net)([0-9\.]+)$", RegexOptions.IgnoreCase)
    let private tryCreateRealVersion (kind : TargetFrameworkKind) (v : string) =
        match SemVer.TryParse v with
        | (true, ver) -> Some { kind = kind; version = ver }
        | _ -> None
        
    let private tryCreateOldVersion (kind : TargetFrameworkKind) (v : string) =
        match System.Int32.TryParse v with
        | (true, _) -> 
            let components = v.ToCharArray() |> Array.map (fun c -> int c - int '0')
            if components.Length = 2 then
                Some { kind = kind; version = SemVer(components.[0], components.[1]) }
            elif components.Length = 3 then
                Some { kind = kind; version = SemVer(components.[0], components.[1], components.[2]) }
            elif components.Length = 4 then
                Some { kind = kind; version = SemVer(components.[0], components.[1], components.[2], components.[3]) }
            else
                None
        | _ -> 
            None

    let tryParse (str : string) =
        let m = rx.Match (str.Trim())
        if m.Success then
            let fw = m.Groups.[1].Value.ToLower()
            let v = m.Groups.[2].Value
            match fw with
            | "netstandard" -> tryCreateRealVersion NetStandard v
            | "netcoreapp" -> tryCreateRealVersion NetCoreApp v
            | _ -> tryCreateOldVersion NetFramework v

        else
            None

    let netstandard (f : TargetFramework) =
        match f.kind with
        | NetStandard -> 
            Some f.version
        | NetFramework ->
            if f.version >= SemVer(4,7,2) then Some <| SemVer(2,0)
            elif f.version >= SemVer(4,6,1) then Some <| SemVer(1,4)
            elif f.version >= SemVer(4,5,1) then Some <| SemVer(1,2)
            elif f.version >= SemVer(4,5) then Some <| SemVer(1,1)
            else None
        | NetCoreApp ->
            if f.version >= SemVer(2,0) then Some <| SemVer(2,0)
            elif f.version >= SemVer(1,0) then Some <| SemVer(1,0)
            else None

    let canReference (project : TargetFramework) (lib : TargetFramework) =
        match project.kind, lib.kind with
        | NetFramework, NetFramework
        | NetCoreApp, NetCoreApp
        | NetStandard, NetStandard ->
            project.version >= lib.version

        | NetStandard, NetCoreApp
        | NetStandard, NetFramework
        | NetCoreApp, NetFramework
        | NetFramework, NetCoreApp -> 
            false

        | _, NetStandard ->
            match netstandard project with
            | Some v -> v >= lib.version
            | None -> false

    let private versionDouble (v : SemVer) =
        if v.Revision >= 0 then float v.Major + 0.1 * (float v.Minor) + 0.01 * (float v.Build) + 0.001 * (float v.Revision)
        elif v.Build >= 0 then float v.Major + 0.1 * (float v.Minor) + 0.01 * (float v.Build) 
        elif v.Minor >= 0 then float v.Major + 0.1 * (float v.Minor) 
        else float v.Major 

    let private score (project : TargetFramework) (lib : TargetFramework) =
        if project.kind = lib.kind then 10000.0 * versionDouble lib.version
        else versionDouble lib.version

    let chooseReference (project : TargetFramework) (available : seq<TargetFramework>) =
        let compatible =
            available 
            |> Seq.filter (canReference project)
            |> Seq.toList

        match compatible with
        | [] -> None
        | [s] -> Some s
        | many -> many |> List.maxBy (score project) |> Some
        

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
        let (s, _) = runCmd "dotnet" ["restore"; file]
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
                
                Log.startTimed "%s" (Path.GetFileName file)
                let dir = Path.GetDirectoryName file
                let netcore, res = projInfo [] file
                match res with
                | Result.Ok [Ok (Inspect.GetResult.FscArgs a)] ->

                    let log (e : ErrorInfo) =
                        match e.severity with
                        | Severity.Debug -> () //Log.line "%s" e.message
                        | Severity.Error -> Log.error "%s" e.message
                        | Severity.Warning -> Log.warn "%s" e.message
                        | Severity.Info -> () //Log.line "%s" e.message

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

                    match Async.RunSynchronously res with
                    | Some generated -> 
                        let old = files |> Seq.map (fun f -> Path.GetFullPath(Path.Combine(dir, f))) |> Set.ofSeq
                        let gen = 
                            generated 
                            |> Array.filter (fun f -> not (Set.contains (Path.GetFullPath f) old)) 
                            |> Array.map Path.GetFileName

                        if gen.Length > 0 then
                            Log.start "generated"
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
