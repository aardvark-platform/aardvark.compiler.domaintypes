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

type DomNode private(tag : string, content : Either<IMod<string>, alist<DomNode>>) =
    member x.Children = content
    member x.Tag = tag

    new(tag : string, content : IMod<string>) = DomNode(tag, Left content)
    new(tag : string, content : alist<DomNode>) = DomNode(tag, Right content)


let treeView (view : 'a -> DomNode) (t : MTree<'a, _>) =
    
    let rec nodeView (n : MNode<'a, _>) =
        DomNode(
            "li",
            AList.ofList [
                view n.value
                DomNode(
                    "ul",
                    n.children |> AList.map nodeView
                )
            ]
        )

    DomNode(
        "ul",
        t.roots |> AList.map nodeView
    )


[<EntryPoint>]
let main argv =
    let s : MMyState = failwith ""

    let v = s.blubber

    let intView = 
        s.intTree |> treeView (fun v -> DomNode("span", Mod.map string v))

    let thingView = 
        s.tree |> treeView (fun v -> DomNode("span", v.name))

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
