﻿namespace DomainModel

open Aardvark.Base
open Aardvark.Base.Incremental

type Model =
    {
        fileName    : string
        bounds      : Box3d
    }

[<DomainType>]
type MyUnion =
    | A of int * hset<int>
    | B of string
    | C

[<DomainType>]
type Object =
    {
        [<PrimaryKey>]
        name        : string
        sepp        : string
        trafo       : Trafo3d
        model       : Model
        blubber     : plist<MyUnion>
    }

[<DomainType>]
type Symbol = V2i of int 

[<DomainType>]
type U = {
    [<PrimaryKey>]
    id : int 
  }

[<DomainType>]
type U2 = { 
    [<PrimaryKey>]
    id : int 
 } // duplcate

[<DomainType>]
type State =
    {
        [<TreatAsValue>]
        past        : Option<State>

        [<NonIncremental>]
        hugo : int

        primary     : Option<Object>
        viewTrafo   : Trafo3d
        objects     : hset<Object>
        [<TreatAsValue>]
        test        : plist<Object>

        [<NonIncremental>]
        threads     : hmap<int, Object>

        testBlub : option<int>

        shouldBeU : hset<U>
    }

[<DomainType>]
type Tree =
    {
        name : string
        id : plist<Tree>
    }

namespace Groups

open Aardvark.Base
open Aardvark.Base.Incremental

[<DomainType>]
type Researcher = { urdar : int }

[<DomainType>]
type Manager = { gabbl : int }

[<DomainType>]
type ArrWr = { arr : array<int>; gumml : int * int }

[<DomainType>]
type TupleUnion =
    | Knubbl of int * int * int
    | Gabbl of char

[<DomainType>]
type TupleUnion2 =
    | Knubbl2 of a : int * b : int * c : int // need to be named differently
    | Gabbl2 of super : char

//[<DomainType>]
//type TTree<'a> =
//    | TLeaf of 'a
//    | TNode of 'a * TTree<'a>

[<DomainType>]
type Generic<'a> = { gabbl : hset<'a> }