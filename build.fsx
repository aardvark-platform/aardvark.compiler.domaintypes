#load @"paket-files/build/vrvis/Aardvark.Fake/DefaultSetup.fsx"
#r "System.Linq.dll"

open Fake
open System
open System.IO
open System.Diagnostics
open Aardvark.Fake


do Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

DefaultSetup.install ["src/Aardvark.Compiler.DomainTypes.sln"]

Target "MergeDotNet" (fun () ->
    
    let outFolder = 
        if config.debug then Path.GetFullPath(Path.Combine("bin", "Debug", "netstandard2.0"))
        else Path.GetFullPath(Path.Combine("bin", "Release", "netstandard2.0"))
        
    CopyFiles outFolder (!!Path.Combine("packages", "FSharp.Compiler.Service", "lib", "netstandard1.6", "**"))
    CopyFiles outFolder (!!Path.Combine("packages", "FSharp.Core", "lib", "netstandard1.6", "**"))

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
        let args (i : ProcessStartInfo) =
            i.FileName <- Path.GetFullPath(Path.Combine("packages", "build", "ILRepack", "tools", "ILRepack.exe"))
            i.Arguments <- String.concat " " args
            i.WorkingDirectory <- outFolder
            ()
        ProcessHelper.execProcess args TimeSpan.MaxValue
    if worked then
        tracefn "merged"
    else
        failwith "error in ILRepack"
)

Target "MergeVS" (fun () ->
    
    let outFolder = 
        if config.debug then Path.GetFullPath(Path.Combine("bin", "Debug"))
        else Path.GetFullPath(Path.Combine("bin", "Release"))
      
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
        let args (i : ProcessStartInfo) =
            i.FileName <- Path.GetFullPath(Path.Combine("packages", "build", "ILRepack", "tools", "ILRepack.exe"))
            i.Arguments <- String.concat " " args
            i.WorkingDirectory <- outFolder
            ()
        ProcessHelper.execProcess args TimeSpan.MaxValue
    if worked then
        tracefn "merged"
    else
        failwith "error in ILRepack"
)

Target "Merge" (fun () -> ())


"Compile" ==> "MergeVS"
"Compile" ==> "MergeDotNet"
"MergeVS" ==> "Merge"
"MergeDotNet" ==> "Merge"
"Merge" ==> "CreatePackage"

entry()