#load @"paket-files/build/vrvis/Aardvark.Fake/DefaultSetup.fsx"
#r "System.Linq.dll"

open Fake
open System
open System.IO
open System.Diagnostics
open Aardvark.Fake


do Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

DefaultSetup.install ["src/Aardvark.Compiler.DomainTypes.sln"]

//Target "BuildTargets" (fun _ -> 
//    Fake.MSBuildHelper.bu
//)


entry()