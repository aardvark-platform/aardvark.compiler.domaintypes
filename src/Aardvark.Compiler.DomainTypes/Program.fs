namespace Program

open System.IO


module Main = 
    open Aardvark.Compiler.DomainTypes


    [<EntryPoint>]
    let main args =
        let task = Preprocess()
        let item = Path.Combine(__SOURCE_DIRECTORY__, @"..\Example\DomainModel.fs")
        task.Current <- item
        task.Item <- item
        task.Execute() |> ignore
        0
