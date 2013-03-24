
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

let runPrint p str =
    let result = run p str

    match result with
        | Success(result, _, _) ->
            let sql = cross result
            let optimizedSql = cross <| optimize result

            result |> sprintf "%A" |> printn
            printn ""
            printn sql

            if optimizedSql <> sql then
                printn optimizedSql
        
        | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg

    printn ""

let MAX_SEQUENTIAL_EMPTY_REPL_LINES = 1
let readInputLines() = 
    let rec readInputLines' emptyLineStreak =
        seq {
            let input = Console.ReadLine()
            let thisLineEmpty = String.IsNullOrWhiteSpace input

            let emptyLineStreak' = if thisLineEmpty then emptyLineStreak + 1 else 0

            if emptyLineStreak' < MAX_SEQUENTIAL_EMPTY_REPL_LINES then
                yield input
                yield! readInputLines' emptyLineStreak'
        }

    readInputLines' -1

let rec repl() =
    Console.Write("ProtoSql> ")
    let input = readInputLines()
    let query = String.Join("\n", input)
    runPrint protoSqlParser query
    repl()

[<EntryPoint>]
let main args =
    let testing = false
    if testing then
        test()
    else
        repl()

    Console.ReadLine() |> ignore
    0
