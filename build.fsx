open Fake.IO

#r "paket: groupref Build //"
#load ".fake/build.fsx/intellisense.fsx"
#load @"paket-files/build/aardvark-platform/aardvark.fake/DefaultSetup.fsx"

open Fake
open System
open System.IO
open System.Diagnostics
open Aardvark.Fake
open Fake.Core.TargetOperators
open Fake.Core
open Fake.IO
open Fake.IO.Globbing.Operators


do Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

DefaultSetup.install ["src/Aardvark.Compiler.DomainTypes.sln"]

Target.create "MergeDotNet" (fun _ ->
    
    let outFolder = 
        if config.debug then Path.GetFullPath(Path.Combine("bin", "Debug", "netstandard2.0"))
        else Path.GetFullPath(Path.Combine("bin", "Release", "netstandard2.0"))
        
    Shell.copyFiles outFolder (!!Path.Combine("packages", "FSharp.Compiler.Service", "lib", "netstandard1.6", "**"))
    Shell.copyFiles outFolder (!!Path.Combine("packages", "FSharp.Core", "lib", "netstandard1.6", "**"))
    Trace.logfn "out folder: %s" outFolder

    let args =
        [|
            "/out:../Aardvark.Compiler.DomainTypes.MSBuild.DotNet.dll"
            "/internalize"
            
            "Aardvark.Compiler.DomainTypes.MSBuild.dll"
            "Aardvark.Compiler.DomainTypes.Build.dll"
            "../../../packages/FSharp.Compiler.Service/lib/netstandard2.0/FSharp.Compiler.Service.dll"
            "../../../packages/FSharp.Core/lib/netstandard1.6/FSharp.Core.dll"
        |]

    let worked = 
        let paramters = 
            { 
                Program = Path.GetFullPath(Path.Combine("packages", "build", "ILRepack", "tools", "ILRepack.exe"))
                WorkingDir = outFolder
                CommandLine = String.concat " " args
                Args = []
            }

        Fake.Core.Process.shellExec paramters
    if worked = 0 then
        Trace.log "merged"
    else
        failwith "error in ILRepack"
)

Target.create "MergeVS" (fun _ ->
    
    let outFolder = 
        if config.debug then Path.GetFullPath(Path.Combine("bin", "Debug"))
        else Path.GetFullPath(Path.Combine("bin", "Release"))
     
    Trace.logfn "out folder: %s" outFolder
      
    let args =
        [|
            "/out:Aardvark.Compiler.DomainTypes.MSBuild.VisualStudio.dll"
            "/internalize"
            "Aardvark.Compiler.DomainTypes.MSBuild.VS.dll"
            "Aardvark.Compiler.DomainTypes.Build.dll"
            "FSharp.Compiler.Service.dll"
            "FSharp.Core.dll"
        |]

    let worked = 
        let paramters = 
            { 
                Program = Path.GetFullPath(Path.Combine("packages", "build", "ILRepack", "tools", "ILRepack.exe"))
                WorkingDir = outFolder
                CommandLine = String.concat " " args
                Args = []
            }

        Fake.Core.Process.shellExec paramters
    if worked = 0 then
        Trace.log "merged"
    else
        failwith "error in ILRepack"
)

Target.create "Merge" (fun _ -> ())


"Compile" ==> "MergeVS"
"Compile" ==> "MergeDotNet"
"MergeVS" ==> "Merge"
"MergeDotNet" ==> "Merge"
"Merge" ==> "CreatePackage"

entry()