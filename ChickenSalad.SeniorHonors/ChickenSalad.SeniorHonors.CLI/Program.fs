
open System
open System.IO
open System.Linq
open Common
open Cross
open FParsec
open Optimize
open Parse
open Test

[<Literal>]
let MAX_SEQUENTIAL_EMPTY_REPL_LINES = 1

[<Literal>]
let REPL_PROMPT = "ProtoSQL> "

let runPrint p str =
    let result = run p str

    match result with
        | Success(result, _, _) ->
            let sql = cross result
            let optimizedSql = cross <| optimize result

            printfn "%A" result
            printn ""
            printn sql

            if optimizedSql <> sql then
                printn ""
                printn optimizedSql
        
        | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg

    printn ""

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
    Console.Write(REPL_PROMPT)
    let input = readInputLines()
    let query = String.Join("\n", input)
    runPrint protoSqlParser query
    repl()

let usage() = "Usage: <executable> [?] [help] [files to convert...]\nTests and a REPL will be run if no arguments\n"

[<EntryPoint>]
let main args = 
    if args.Length = 0 then
        test()
        repl()
    else if args.Any(fun arg -> let arg' = arg.ToLower() in arg' = "?" || arg' = "help") then
        usage() |> printn
    else
        for path in args do
            let text = File.ReadAllText(path)

            let parseResult = run protoSqlParser text
            match parseResult with
                | Success(ast, _, _) ->
                    let sql = ast |> optimize |> cross
                    let path' = Path.ChangeExtension(path, "sql")
                    File.WriteAllText(path', sql)
                | Failure(error, _, _) ->
                    printfn "Failure: %s" error
    0
