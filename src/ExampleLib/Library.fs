namespace ExampleLib

open Aardvark.Base
open Aardvark.Base.Incremental


[<DomainType>]
type MyDomainType =
    {
        a : int
        b : hset<int>
    }
