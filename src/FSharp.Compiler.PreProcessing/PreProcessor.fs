namespace FSharp.Compiler.Preprocessing

open System
open System.IO
open Microsoft.Build.Framework
open Microsoft.Build.Utilities


module private Preprocessing =
    
    let lines =
        [
            "namespace Blubber"
            "type Seppy() ="
            "    inherit Test.BaseType()"
            "    member x.A = 10"
        ]

    [<CompiledName("Run")>]
    let run (fileName : string) (outputFile : string) =
        File.WriteAllLines(outputFile, lines)

type Preprocess() =
    inherit Task()

    let mutable item = ""
    let mutable current = ""
    let mutable results = [||]

    override x.Execute() =
        let all = current.Split([|';'|], StringSplitOptions.RemoveEmptyEntries) |> Set.ofArray

        if Set.contains item all then
            let fileName = Path.GetFileNameWithoutExtension item
            let domFile = fileName + ".g.fs"
            Preprocessing.run item domFile

            results <- [|item; domFile|]
        else
            results <- [|item|]

        true

    [<Required>]
    member x.Item
        with get() = item
        and set i = item <- i

    [<Required>]
    member x.Current
        with get() = current
        and set c = current <- c

    [<Output>]
    member x.Results
        with get() = results
        and set r = results <- r