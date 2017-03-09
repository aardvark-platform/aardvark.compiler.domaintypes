namespace Scratch.DomainTypes.TranslateController

open System
open Aardvark.Base
open Aardvark.Base.Incremental



//[<DomainType>]
type MyOption<'a> = MyNone | MySome of 'a

[<DomainType>]
type Model = {
    hovered           : Option<int>
    trafo             : Trafo3d
}

[<DomainType>]
type Scene = 
    {
        camera : int
        scene : MyOption<Model>
    }


//module SimpleDrawingApp =
//
//    type Polygon = list<V3d>
//
//    type OpenPolygon = {
//        cursor         : Option<V3d>
//        finishedPoints : list<V3d>
//    }
//    
//    [<DomainType>]
//    type Model = {
//        finished : pset<Polygon>
//        working  : Option<OpenPolygon>
//    }
//
//module PlaceTransformObjects =
//
//    [<DomainType>]
//    type Model = {
//        objects : list<Trafo3d>
//        hoveredObj : Option<int>
//        selectedObj : Option<int * TranslateController.Model>
//    }
//
//module Interop =
//    
//    type Active = RenderControl | Gui
//
//    type Scene = {
//        camera : int
//        obj    : V3d
//    }
//
//    type Model = {
//        currentlyActive : Active
//        scene : Scene
//    }


//module SharedModel =
//    
//    [<DomainType>]
//    type TranslateModel = {
//        hovered           : Option<TranslateController.Axis>
//        activeTranslation : Option<Plane3d * V3d>
//        trafo             : Trafo3d
//    }
//
//    [<DomainType>]
//    type TranslateScene = 
//        {
//            camera : int
//            scene : TranslateModel
//        }
//
//    [<DomainType>]
//    type Ui = { cnt : int; info : string }
//
//    [<DomainType>]
//    type Model = { ui : Ui; scene : TranslateScene }
//
//module SharedModel2 =
//   
//
//    [<DomainType>]
//    type Ui = { cnt : int; info : string }
//
//    [<DomainType>]
//    type Model = { ui : Ui; scene : TranslateController.Scene }
//
//module SharedModel3 =
//   
//    open TranslateController
//
//    [<DomainType>]
//    type Ui = { cnt : int; info : string }
//
//    [<DomainType>]
//    type Model = { ui : Ui; scene : Scene }