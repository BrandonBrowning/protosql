
module Optimize

open Grammar

let isOptimizableOperator ident =
    match ident with
        | "+" | "-" | "*" | "/" -> true
        | _ -> false

let optimizedAdd leftPrim rightPrim =
    match (leftPrim, rightPrim) with
        | (PrimativeInt left, PrimativeInt right) -> left + right |> PrimativeInt |> Some
        | (PrimativeFloat left, PrimativeFloat right) -> left + right |> PrimativeFloat |> Some
        | (PrimativeString left, PrimativeLiteral right) -> left + right |> PrimativeString |> Some
        | (PrimativeInt left, PrimativeFloat right) -> float left + right |> PrimativeFloat |> Some
        | (PrimativeFloat left, PrimativeInt right) -> left + float right |> PrimativeFloat |> Some
        | _ -> None

let rec optimizeValueExpr expr =
    match expr with
        | ValueExprBinOp(ident, leftExpr, rightExpr) ->
            if isOptimizableOperator ident then
                let optimizedLeft = optimizeValueExpr leftExpr
                let optimizedRight = optimizeValueExpr rightExpr
                
                match (optimizedLeft, optimizedRight) with
                    | (ValueExprPrimative leftPrim, ValueExprPrimative rightPrim) ->
                        match ident with
                            | "+" -> 
                                let addResult = optimizedAdd leftPrim rightPrim
                                if Option.isSome addResult then
                                    Option.get addResult |> ValueExprPrimative
                                else
                                    ValueExprBinOp(ident, optimizedLeft, optimizedRight)
                            | _ -> ValueExprBinOp(ident, optimizedLeft, optimizedRight)
                    | _ -> ValueExprBinOp(ident, optimizedLeft, optimizedRight)
            else
                expr
        | _ -> expr

let optimizeFrom = id
let optimizeWhere = id
let optimizeOrderBy = id

let optimizeSelectLine line =
    match line with
        | SelectLineExpr(ident, valueExpr) -> SelectLineExpr(ident, optimizeValueExpr valueExpr)
        | _ -> line

let optimizeSelect = List.map optimizeSelectLine

let optimize query = 
    match query with
        Query(from, wheres, orderBy, select) ->
            let optimizedFrom = optimizeFrom from
            let optimizedWheres = Option.bind (List.map optimizeWhere >> Option.Some) wheres
            let optimizedOrderBy = Option.bind (optimizeOrderBy >> Option.Some) orderBy
            let optimizedSelect = Option.bind (optimizeSelect >> Option.Some) select

            Query(optimizedFrom, optimizedWheres, optimizedOrderBy, optimizedSelect)