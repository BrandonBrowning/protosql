
module Parsing
open FParsec

type Parser<'t> = Parser<'t, unit>

let chr = pchar

let str = pstring
let str_ws s = str s >>. spaces
let ws_string s = spaces >>. str s

/// `seperate sep p` is an alias for `sepBy p sep`, which parses parses p (sep p)*
let seperate sep p = sepBy p sep

let betweenChr cOpen cClose p = between (chr cOpen) (chr cClose) p
let betweenStr sOpen sClose p = between (str sOpen) (str sClose) p
let csv p = sepBy (p .>> spaces) (str "," .>> spaces)

let printn (str: string) = printfn "%s" str