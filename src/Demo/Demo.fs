namespace Demo

open Aardvark.Base
open Aardvark.Base.Incremental

[<DomainType>]
type Thing = { [<PrimaryKey>] value : int }

[<DomainType>]
type Example = 
    {
        value : int
        hset : hset<int>
        hsetM : hset<Thing>
        plist : plist<Thing>
        map : hmap<int,Thing>
        list : list<Thing>
        plainArray : array<int>
    }

[<DomainType>]
type UnionExample =
    | Thrice of int * int * int
    | Once

[<DomainType>]
type GenericExample<'a> = { single : 'a; many : hset<'a>}