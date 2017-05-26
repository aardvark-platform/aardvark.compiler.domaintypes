// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.
open Aardvark.Base
open Aardvark.Base.Incremental
open DomainModel
open System.Threading
open System.Runtime.CompilerServices


open DomainModel2


//let visit (s : MMyState) =
//    let tree : MTree<IMod<int>, int> = s.tree
//    let roots : alist<MNode<IMod<int>, int>> = tree.roots

//    roots |> AList.map (fun (v : MNode<IMod<int>, int>) ->
//        let value : IMod<int> = v.value
//        let children : alist<MNode<IMod<int>, int>> = v.children
//        v
//    ) |> ignore

//    ()

[<EntryPoint>]
let main argv =
    let s : MMyState = failwith ""

    let v = s.intTree

    let rec ft (n : MNode<MThing, MThing>) =
        alist {
            yield! AList.ofModSingle n.value.id
            for c in n.children do
                yield! ft c
        }


    let rec fi (n : MNode<IMod<int>, int>) =
        alist {
            yield! AList.ofModSingle n.value
            for c in n.children do
                yield! fi c
        }

    0
