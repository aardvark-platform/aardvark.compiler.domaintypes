namespace DomainModel2

open Aardvark.Base
open Aardvark.Base.Incremental
open DomainModel

[<DomainType>]
type Model2 =
    {
        fm : State
    }
