
module Common
open System
open System.Collections.Generic
open FParsec

type Parser<'t> = Parser<'t, unit>

let chr = pchar
let chr_ws c = chr c .>> spaces
let ws_chr c = spaces >>. chr c

let str = pstring
let str_ws s = str s .>> spaces
let ws_string s = spaces >>. str s

/// `seperate sep p` is an alias for `sepBy p sep`, which parses parses p (sep p)*
let seperate sep p = sepBy p sep

let betweenChr cOpen cClose p = between (chr cOpen) (chr cClose) p
let betweenStr sOpen sClose p = between (str sOpen) (str sClose) p
let csv p = sepBy (p .>> spaces) (chr ',' .>> spaces)

let sepByTrail (psep: Parser<_, _>) (p: Parser<'a, _>) =
    fun (s: CharStream<_>) ->
        let initialState = s.State

        let mutable reply = p s
        let mutable good = reply.Status = Ok

        if not good then
            s.BacktrackTo(initialState)
            Reply(List.empty)
        else
            let results = new List<'a>()
            results.Add(reply.Result)

            while good do
                let loopState = s.State
                let seperator_reply = psep s

                if seperator_reply.Status = Ok then
                    let p_reply = p s
                    if p_reply.Status = Ok then
                        results.Add(p_reply.Result)
                    else
                        good <- false
                else
                    good <- false
                    s.BacktrackTo(loopState)

            Reply(List.ofSeq results)

let id x = x
let printn (str: string) = printfn "%s" str
let eprintn (str: string) = eprintfn "%s" str

let newline = Environment.NewLine
let sjoin (sep: string) (lines: #obj seq) = String.Join(sep, lines)

let (<||>) f g = fun x -> f x || g x
let (<&&>) f g = fun x -> f x && g x

let rmatch text pattern = System.Text.RegularExpressions.Regex.IsMatch(text, pattern)