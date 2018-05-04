namespace DomainModel2

open Aardvark.Base
open Aardvark.Base.Incremental
open DomainModel

[<DomainType>]
type Node<'a> = { value : 'a; children : plist<Node<'a>> }

[<DomainType>]
type Tree<'a> = { roots : plist<Node<'a>> }

[<DomainType>]
type Thing = 
    { 
        [<PrimaryKey>]
        id : string
        name: string 
    }

[<DomainType>]
type MyState = 
    { 
        tree : Tree<Thing>
        intTree : Tree<int> 
        blubber : hset<Thing>    
    }


[<DomainType>]
type Tab = { name : string; url : string }

[<DomainType>]
type Tree2 = 
    | Vertical of Tree2 * Tree2
    | Horizontal of Tree2 * Tree2
    | Leaf of Tab

[<DomainType>]
type Model = { 
    tabs : list<Tab>
}

type Action = Action


open Groups

[<DomainType>]
type Persons =
    | Researchers of plist<Researcher>
    | Managers of plist<Manager>
    | BothOida of plist<Researcher> * plist<Manager> * plist<Manager>