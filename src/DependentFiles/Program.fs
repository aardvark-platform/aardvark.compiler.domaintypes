// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.


[<EntryPoint>]
let main argv = 
    let mySepp = Blubber.Seppy()
    printfn "%A" mySepp.A
    0 // return an integer exit code
