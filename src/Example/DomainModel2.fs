namespace DomainModel2

open Aardvark.Base
open Aardvark.Base.Incremental

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