#load @"paket-files/build/vrvis/Aardvark.Fake/DefaultSetup.fsx"
#r "System.Linq.dll"

open Fake
open System
open System.IO
open System.Diagnostics
open Aardvark.Fake


do Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

DefaultSetup.install ["src/Aardvark.Compiler.DomainTypes.sln"]

Target "Merge" (fun _ ->
    ILMerge (fun p ->
        { p with 
            DebugInfo = false
            Libraries = 
            [
                @"bin\Release\FSharp.Compiler.Service.dll"
                @"bin\Release\FSharp.Compiler.Service.MSBuild.v12.dll"
                @"bin\Release\System.Collections.Immutable.dll"
                @"bin\Release\System.Reflection.Metadata.dll"
            ] 
        }
    )  @"bin\Release\Aardvark.Compiler.DomainTypes.Merged.dll" @"bin\Release\Aardvark.Compiler.DomainTypes.dll"
)



entry()