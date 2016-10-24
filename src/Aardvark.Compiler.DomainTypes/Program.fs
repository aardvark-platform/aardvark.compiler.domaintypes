namespace Program


module Main = 
    open Aardvark.Compiler.DomainTypes


    //[<EntryPoint>]
    let main args =
        let task = Preprocess()
        let item = @"C:\Users\Schorsch\Development\aardvark-compiler-domaintypes\src\Example\DomainModel.fs"
        task.Current <- item
        task.Item <- item
        task.Execute() |> ignore
        0
