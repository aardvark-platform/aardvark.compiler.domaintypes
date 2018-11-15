# aardvark.compiler.domaintypes

Generates incremental datastructures (from Aardvark.Base.Incremental) from purely functional input data types. 
Generated types can be used to compute diffs on immutable input values in order to feed changes incrementally into the mutable variants. 
This is heavily used by [aardvark.media](https://github.com/aardvark-platform/aardvark.media). 
Additionally to a standalone source to source compiler this functionality is exposed via a msbuild build step.

# Usage in Code

```[<DomainType>]
type Thing = {  value : int }
```

Creates a type:

```
    type MThing(__initial : Demo.Thing) =
        inherit obj()
        let mutable __current : Aardvark.Base.Incremental.IModRef<Demo.Thing> = Aardvark.Base.Incremental.EqModRef<Demo.Thing>(__initial) :> Aardvark.Base.Incremental.IModRef<Demo.Thing>
        let _value = ResetMod.Create(__initial.value)
        
        member x.value = _value :> IMod<_>
        
        member x.Current = __current :> IMod<_>
        member x.Update(v : Demo.Thing) =
            if not (System.Object.ReferenceEquals(__current.Value, v)) then
                __current.Value <- v
                
                ResetMod.Update(_value,v.value)
                
        
        static member Create(__initial : Demo.Thing) : MThing = MThing(__initial)
        static member Update(m : MThing, v : Demo.Thing) = m.Update(v)
        
        override x.ToString() = __current.Value.ToString()
        member x.AsString = sprintf "%A" __current.Value
        interface IUpdatable<Demo.Thing> with
            member x.Update v = x.Update v
    
    
    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Thing =
        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        module Lens =
            let value =
                { new Lens<Demo.Thing, System.Int32>() with
                    override x.Get(r) = r.value
                    override x.Set(r,v) = { r with value = v }
                    override x.Update(r,f) = { r with value = f r.value }
                }
                
```

By using `Create` a incremental variant can be created from the immutable version. By using `Update` the incremental variant can be updated using a new immutable value. Additionally, lenses are created for the immutable input type.

The usage can be like this:
```
    let thing = { value = 1 }
    let mthing = MThing.Create(thing)
    let adaptiveCode = 
        mthing.value |> Mod.map (sprintf "%A") 

    printfn "%A" (Mod.force adaptiveCode)

    let newThing = { value = 10 }
    transact (fun _ -> mthing.Update(newThing))
    printfn "%A" (Mod.force adaptiveCode)
  ```

A more complicated example shows how translation is done: 

```
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
```

The generated type has multiple fields, depending on the types (i.e. degree of change tracking is guided by the types):

```
    let example =
        {
            value = 0
            hset  = HSet.empty
            hsetM = HSet.empty
            plist = PList.empty
            map   = HMap.empty
            list   = []
            plainArray = [||]
        }

    let mExample = MExample.Create example
    let value : IMod<int>    = mExample.value
    let hset  : aset<int>    = mExample.hset
    let hsetM : aset<MThing> = mExample.hsetM
    let plist : alist<MThing> = mExample.plist
    let map   : amap<int,MThing>  = mExample.map
    let list  : IMod<list<Thing>> = mExample.list
    let plainArray : IMod<array<int>> = mExample.plainArray
```

# Usage in your project

Compilation is fully integrated and transparent to your build pipeline via the [nuget package](https://www.nuget.org/packages/Aardvark.Compiler.DomainTypes.MSBuild/).
