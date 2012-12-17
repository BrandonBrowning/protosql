
module Parsing
open FParsec

type Parser<'t> = Parser<'t, unit>

let str = pstring
let str_ws s = str s >>. spaces
let ws_string s = spaces >>. str s

let betweenStr sOpen sClose p = between (str sOpen) (str sClose) p
let csv p = sepBy (p .>> spaces) (str "," .>> spaces)