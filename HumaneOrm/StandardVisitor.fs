namespace HumaneOrm
open Sql
open Lexer

(*
    The standard pieces of visiting and translating a SQL AST
*)
module StandardVisitor =
    let rec indent (i) = 
        match i with
            | x when x <= 0 -> ""
            | a -> "\t"+indent(a-1)
    //The atomic value
    let visitValue (v:Value) =
        match v with
            | Int x -> sprintf "%d" x
            | Float x -> sprintf "%f" x
            | String x -> sprintf "%s" x
    //The operators
    let visitOp (o:Op) =
        match o with
            | Eq -> "="| Gt -> ">"| Ge -> ">="| Lt -> "<"| Le -> "<="
            | Add -> "+"| Sub -> "-"| Div -> "/"| Mul -> "*"| Pow -> "^"
            | _ -> sprintf "%A" o
    //The join statements
    let visitJoinType (j:JoinType) =
        match j with
            | Right -> "right outer join"| Left -> "left outer join"
            | Inner -> "inner join"| InnerApply -> "inner apply"
            | OuterApply -> "outer apply"| CrossApply -> "cross apply"
            | _ -> sprintf "%A" j
    //The expression tree
    let rec visitExpressionTree (expressionTree:ScalarExpression, level) =
        match expressionTree with
            | Atom(a)  -> visitValue a
            | Unary(Count,e1) -> sprintf "count(%s)" (visitExpressionTree (e1,level)) 
            | Unary(op,e1) -> sprintf "%s.%s" (visitOp op) (visitExpressionTree (e1, level)) 
            | Cast(e,v) -> sprintf "cast(%s as %s)" (visitExpressionTree (e, level)) (visitValue v)
            | CastWithPrecision(e,v,v2) -> sprintf "cast(%s as %s(%s))" (visitExpressionTree (e,level)) (visitValue v) (visitValue v2)
            | Binary(NameScope,e1,e2) -> sprintf "%s.%s" (visitExpressionTree (e1, level)) (visitExpressionTree (e2, level))
            | Binary(op,e1,e2) -> sprintf "(%s %s %s)" (visitExpressionTree (e1, level)) (visitOp op) (visitExpressionTree (e2, level))
            | ScalarExpressionSubquery(s) -> sprintf "(\n%A\n%s) as %s,\n" (visitQuery ((fst s), (level + 1))) (indent level) "??"//(visitValue (snd s))
            | _ -> "??visitExpressionTree"
    //the column list
    and visitColumnSelectList(cl: Column list, level) =
        List.fold (fun(acc: string) (c:Column) -> 
                match c with 
                | Expression(s,v) -> acc + (indent level) + sprintf "%s as %s,\n" (visitExpressionTree (s, level)) (visitValue v)
        ) "select\n" cl
    //the join table list
    and visitTableJoinList(jl: Join list, level) =
        List.fold(fun(acc:string) (j:Join) ->
            match j with
            | (Table(v1, v2), jt, None) -> acc + (indent level) + sprintf "%s %s as %s\n" (visitJoinType jt) (visitValue v1) (visitValue v2)
            | (Table(v1, v2), jt, Some exp) -> acc + (indent level) + sprintf "%s %s as %s\n %son (%s)" (visitJoinType jt) (visitValue v1) (visitValue v2) (indent level) (visitExpressionTree (exp, level))
            | (TableSubquery(sql, v), jt, Some exp) -> acc + (indent level) + sprintf "%s (\n%s\n%s) as %s\n on (%s)" (visitJoinType jt) (visitQuery (sql, (level + 1))) (indent (level+1)) (visitValue v) (visitExpressionTree (exp, level))
            | (TableSubquery(sql, v), jt, None) -> acc + (indent level) + sprintf "%s (\n%s\n%s) as %s\n" (visitJoinType jt) (visitQuery (sql, (level + 1))) (indent level) (visitValue v) 
            | _ -> "??visitTableJoinList" 
        ) "" jl
    //the initial table
    and visitJoinTable(jt: JoinTable, level) =
        (indent (level-1))+"from \n" + match jt with
        | Table(v1, v2) -> sprintf "%s%s as %s\n" (indent level) (visitValue v1) (visitValue v2)
        | TableSubquery(sql, v)-> sprintf "%s(\n%s\n%s) as %s\n" (indent level) (visitQuery (sql, (level + 1))) (indent level) (visitValue v)
    //the sql query
    and visitQuery (sql:SqlStatement, level) =
        (indent level) + (visitColumnSelectList (sql.Columns, (level + 1))) + (visitJoinTable (sql.Table, (level + 1))) + (visitTableJoinList (sql.Joins, (level + 1)))
    and visitSqlQuery (sql:SqlStatement) =
        (visitQuery (sql, 0)).Replace("\"","")
