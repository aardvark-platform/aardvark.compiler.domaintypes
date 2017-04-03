// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.
open Aardvark.Base
open Aardvark.Base.Incremental
open DomainModel
open System.Threading

let test () =
    
    let mo =     
        {
            fileName    = "sadsad"
            bounds      = Box3d.Unit
        }

    let o = 
        { 
            name = "obj"
            trafo = Trafo3d.Identity
            model = mo 
        }

    let state = 
        { 
            past = None
            primary = Some o
            viewTrafo = Trafo3d.Identity
            objects = HSet.empty
            test = PList.empty
            threads = ThreadPool.create()
        }

    let mstate = MState.Create state

    mstate.objects |> ASet.unsafeRegisterCallbackKeepDisposable (fun deltas ->
        printfn "delta: %A" deltas

        for d in deltas do
            match d with
                | Add(_,v) -> 
                    v.trafo |> Mod.unsafeRegisterCallbackKeepDisposable (fun t -> 
                        printfn "trafo(%s): %A" (Mod.force v.name) t.Forward.Det
                    ) |> ignore
                | _ -> 
                    ()

    ) |> ignore

    printfn "add obj"
    let state = { state with objects = HSet.add o state.objects }
    transact (fun () -> mstate.Update state )

    printfn "set trafo (scale 3.0)"
    let n = { o with trafo = Trafo3d.Scale 3.0 }
    let state = { state with objects = HSet.remove o (HSet.add n state.objects) }
    transact (fun () -> mstate.Update state )
    let o = n
    
    // TODO: remove missing here
    printfn "change name and set trafo (scale 6.0)"
    let n = { o with name = "hugo"; trafo = Trafo3d.Scale 6.0 }
    let state = { state with objects = HSet.remove o (HSet.add n state.objects) }
    transact (fun () -> mstate.Update state )
    let o = n


    System.Environment.Exit 0
    ()

open System.IO

let testUnion() =
    let u = A(1,HSet.empty)
    let sepp = MMyUnion.Create u

    let test =
        adaptive {
            let! sepp = sepp
            match sepp with
                | MA (x,y) -> return 2
                | MB str -> return 54
                | MC -> return 2
        }

    sepp |> Mod.unsafeRegisterCallbackKeepDisposable (fun c -> printfn "case changed: %A" c) |> ignore

    printfn "update a"
    let u = A(2,HSet.empty)
    transact (fun () -> sepp.Update u)

    printfn "change to b"
    let u = B "asdsad"
    transact (fun () -> sepp.Update u)

    printfn "change to c"
    let u = C
    transact (fun () -> sepp.Update u)
    
    System.Environment.Exit 0
    
open System.Threading.Tasks
open System.Runtime.CompilerServices


let listy =
    proclist {
        for i in 1 .. 100 do
            do! Proc.Sleep 100
            yield i
    }

let testy (sem : SemaphoreSlim) (t : Task<int>) =
    proc {
        try
            //printfn "before: %A" Thread.CurrentThread.ManagedThreadId
            do! Proc.SwitchToNewThread()
            try
                printfn "thread: %A" Thread.CurrentThread.ManagedThreadId
                do! sem.Await()
                printfn "thread: %A" Thread.CurrentThread.ManagedThreadId
                let! v = t
                return 2 * v
            with e ->
                printfn "exn %A" (e.GetType())
        finally
            printfn "cleanup"
    }


[<EntryPoint>]
let main argv =

    let rec subscribe (ct : CancellationToken) (f : 'a -> unit) (l : ProcList<'a, unit>) =
        let s = { ct = ct }
        l.run.ContinueWith s (fun res ->
            match res with
                | Faulted e -> printfn "faulted %A" e
                | Cancelled -> printfn "cancelled"
                | Value v -> 
                    match v with
                        | Nil -> ()
                        | Cons(h,t) ->
                            f h
                            subscribe ct f t
        )

    let cancel = new CancellationTokenSource()
    listy |> subscribe cancel.Token (printfn "value: %A")
    System.Console.ReadLine() |> ignore
    cancel.Cancel()
    System.Console.ReadLine() |> ignore
    System.Environment.Exit 0

    use sem = new SemaphoreSlim(0)

    let v = new TaskCompletionSource<int>()
    let cancel = new CancellationTokenSource()
    printfn "start"
    let a = Proc.StartAsTask(testy sem v.Task , cancellationToken = cancel.Token)
    sem.Release() |> ignore
    cancel.Cancel()
    printfn "end: %A" (a.Result)
    printfn ""
    
    let v = new TaskCompletionSource<int>()
    let cancel = new CancellationTokenSource()
    printfn "start"
    let a = Proc.StartAsTask(testy sem v.Task , cancellationToken = cancel.Token)
    sem.Release() |> ignore
    v.SetCanceled()
    printfn "end: %A" (a.Result)
    printfn ""
    
    let v = new TaskCompletionSource<int>()
    let cancel = new CancellationTokenSource()
    printfn "start"
    let a = Proc.StartAsTask(testy sem v.Task , cancellationToken = cancel.Token)
    sem.Release() |> ignore
    v.SetResult 3
    printfn "end: %A" (a.Result)
    printfn ""
    
    let v = new TaskCompletionSource<int>()
    let cancel = new CancellationTokenSource()
    printfn "start"
    let a = Proc.StartAsTask(testy sem v.Task , cancellationToken = cancel.Token)
    sem.Release() |> ignore
    v.SetException (System.ArgumentException "asdasd")
    printfn "end: %A" (a.Result)
    printfn ""
    
    let v = new TaskCompletionSource<int>()
    let cancel = new CancellationTokenSource()
    printfn "start"
    let a = Proc.StartAsTask(testy sem v.Task , cancellationToken = cancel.Token)
    Thread.Sleep(100)
    sem.Dispose()
    v.SetResult(5)
    printfn "end: %A" (a.Result)
    printfn ""
    System.Console.ReadLine() |> ignore

    //test()
    //testUnion()

    0
