﻿
module Main

open System
open System.Collections.Generic
open Common
open Cross
open FParsec
open Grammar
open Optimize
open Parse
open Test

let tabify (s: string) =
    "\t" + s.Replace("\r\n", "\r\n\t")

let runPrint p str =
    let result = run p str

    match result with
        | Success(result, _, _) ->
            let sql = cross result
            let optimizedSql = cross <| optimize result

            printn "Parsed"
            result |> sprintf "%A" |> tabify |> printn
            printn "Translated"
            tabify sql |> printn

            if optimizedSql <> sql then
                printn "Optimized"
                printn <| tabify optimizedSql
        
        | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg

    printn ""

let rec readInputLines() = 
    seq {
        let input = Console.ReadLine()
        if not <| String.IsNullOrEmpty input then
            yield input
            yield! readInputLines()
    }

let rec repl() =
    Console.Write("ProtoSql> ")
    let input = readInputLines()
    let query = String.Join("\n", input)
    runPrint protoSqlParser query
    repl()

[<EntryPoint>]
let main args =
    let testing = true
    if testing then
        test()
    else
        repl()

    Console.ReadLine() |> ignore
    0
