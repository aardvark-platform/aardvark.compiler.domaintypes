﻿namespace Aardvark.Compiler.DomainTypes

open System
open System.Text
open Microsoft.FSharp.Compiler
open Microsoft.FSharp.Compiler.Range

type ErrorInfo =
    {
        file        : string
        startLine   : int
        endLine     : int
        startColumn : int
        endColumn   : int
        message     : string
        code        : int
    }

type Result<'a> = 
    | Success of 'a
    | Error of ErrorInfo

type CodeGenState = 
    { 
        result : StringBuilder
        indent : string 
        warnings : list<ErrorInfo>
    }
        
type CodeGen<'a> = { generate : CodeGenState -> CodeGenState * Result<'a> }

type CodeGenBuilder() =
    static let instance = CodeGenBuilder()

    static member Instance = instance

    member x.Bind(m : CodeGen<'a>, f : 'a -> CodeGen<'b>) =
        { generate = fun s ->
            match m.generate s with
                | s, Success(v) -> f(v).generate s
                | s, Error(e) -> s, Error(e)
        }

    member x.Return(v : 'a) =
        { generate = fun s ->
            s, Success v
        }

    member x.ReturnFrom(v : CodeGen<'a>) = v

    member x.Zero() =
        { generate = fun s ->
            s, Success(())
        }

    member x.Delay(f : unit -> CodeGen<'a>) =
        { generate = fun s ->
            f().generate s
        }

    member x.For(e : seq<'a>, f : 'a -> CodeGen<unit>) =
        { generate = fun s ->
            use e = e.GetEnumerator()
            let rec run(s) =
                if e.MoveNext() then
                    match f(e.Current).generate s with
                        | s, Success(()) -> run s
                        | s, Error(e) -> s, Error(e)
                else
                    s, Success(())
            run s
        }

    member x.While(guard : unit -> bool, body : CodeGen<unit>) =
        { generate = fun s ->
            let rec run s =
                if guard() then
                    match body.generate s with
                        | s, Success(()) -> run s
                        | s, Error(e) -> s, Error e
                else
                    s, Success(())
            run s
        }
    
    member x.Combine(l : CodeGen<unit>, r : CodeGen<'a>) =
        { generate = fun s ->
            match l.generate s with
                | s, Success(()) ->
                    r.generate s
                | s, Error e ->
                    s, Error e
        }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module CodeGen =
    let line fmt = 
        let inner (str : string) =
            { generate = fun s ->
                s.result.AppendLine(s.indent + str) |> ignore
                s, Success(())
            }
        Printf.kprintf inner fmt

    let error (code : int) (range : range) (fmt : Printf.StringFormat<'a, CodeGen<'x>>) : 'a =
        Printf.kprintf (fun str ->
            { generate = fun s ->
                s, Error {
                    file        = range.FileName
                    startLine   = range.StartLine
                    endLine     = range.EndLine
                    startColumn = range.StartColumn
                    endColumn   = range.EndColumn
                    message     = str
                    code        = code
                }
            }
        ) fmt

    let warn (code : int) (range : range) (fmt : Printf.StringFormat<'a, CodeGen<unit>>) : 'a =
        Printf.kprintf (fun str ->
            { generate = fun s ->
                let info = 
                    {
                        file        = range.FileName
                        startLine   = range.StartLine
                        endLine     = range.EndLine
                        startColumn = range.StartColumn
                        endColumn   = range.EndColumn
                        message     = str
                        code        = code
                    }
                {s with warnings = s.warnings @ [info] }, Success ()
            }
        ) fmt

    let push = { generate = fun s -> { s with indent = s.indent + "    " },Success() }
    let pop = { generate = fun s -> { s with indent = s.indent.Substring 4 }, Success() }
            

    type InnerBuilder(str : string) =
        inherit CodeGenBuilder()

        member x.Run(g : CodeGen<'a>) =
            CodeGenBuilder.Instance {
                do! line "%s" str
                do! push
                let! res = g
                do! pop
                return res
            }

    let inline scope fmt = Printf.kprintf InnerBuilder fmt

    let run (g : CodeGen<unit>) =
        let res = 
            g.generate {
                result = System.Text.StringBuilder()
                indent = ""
                warnings = []
            }
        match res with
            | s, Success(()) -> s.result.ToString()
            | s, Error e -> failwithf "%A" e

[<AutoOpen>]
module ``CodeGenerator Builder`` =
    let codegen = CodeGenBuilder.Instance

    module List =
        let rec mapC (f : 'a -> CodeGen<'b>) (l : list<'a>) =
            codegen {
                match l with
                    | [] -> return []
                    | h :: t ->
                        let! h = f h
                        let! t = mapC f t
                        return h :: t
            }