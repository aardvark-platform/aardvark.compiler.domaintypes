namespace Program


module Main = 
    open Aardvark.Compiler.DomainTypes


    [<EntryPoint>]
    let main args =
        let task = Preprocess()
        let item = @"C:\Development\diffgenerator\src\Example\DomainModel.fs"
        task.Current <- item
        task.Item <- item
        task.Execute() |> ignore
        0
