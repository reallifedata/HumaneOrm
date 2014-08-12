namespace HumaneOrm
open System
open System.Text
open System.Text.RegularExpressions
open Sql
open Lexer
open Microsoft.FSharp.Text.Lexing

module Program =
    let test01 = "   
                SELECT x, y, z   
                FROM t1   
                LEFT JOIN t2   
                INNER JOIN t3 ON t3.ID = t2.ID   
                WHERE x = 50 AND y = 20  AND z='bar'
                ORDER BY x ASC, y DESC, z   
            "   
    let test02 = "
            exec sp_executesql N'SELECT 
[GroupBy1].[A1] AS [C1]
FROM ( SELECT 
    COUNT(1) AS [A1]
    FROM [dbo].[Tabulka] AS [Extent1]
    WHERE ([Extent1].[Column] LIKE @p__linq__0 ESCAPE N''~'') AND ([Extent1].[Column] LIKE @p__linq__1 ESCAPE N''~'')
)  AS [GroupBy1]',N'@p__linq__0 nvarchar(4000),@p__linq__1 nvarchar(4000)',@p__linq__0=N'%text1%',@p__linq__1=N'%text2%'
            "
    let test03 = "
        select top(100)
            x
            ,x as namedVal
            ,x as [namedValBracket]
            ,[x]
            ,[table].[column]
            ,[x] as namedValWithBracket
            ,[x] as [namedValBracketWithBracket]
            ,(paren1)
            ,((paren2))
            ,((b ^ 2) - (4 * a * c)) / (4 * a ^ 2) as Quadratic
            ,count(1) as CountOne
            ,count( foo )
            ,(select a from baz) as SubqueryTest
            ,(select b from foobar) as [SubqueryTestWithBracket]
        from
            (select distinct bazz from foo) as theTable
            outer apply (select [a] from bar where 1 = 1) as table2
            right outer join Table3 
                ON a LIKE '%b%'
    "
    let stripSpExecuteSql (input:string) =
        if not(input.Trim().StartsWith("exec sp_executesql")) then 
            input.Trim()
        else
            if not(input.Contains("@p__linq__0")) then
                input.Trim().Replace("exec sp_executesql N'","").TrimEnd([|'\''|]).Replace("''","'")
            else
                let stripped = input.Trim().Replace("exec sp_executesql N'","") |> (fun x -> x.Substring(0, x.IndexOf("]',")+1).Replace("''","'"))
                (new Regex(@"@p__linq__[\d]+=[^,]+")).Matches(input) 
                    |> Seq.cast
                    |> Seq.map(fun x-> x:> System.Text.RegularExpressions.Match)
                    |> Seq.map(fun x-> ((x.Value.Split [|'='|]).[0].Trim(), (x.Value.Split [|'='|]).[1].Trim()))
                    |> Seq.fold(fun (acc:string) (k, v) -> acc.Replace(k,v)) stripped

    let visitValue (v:Value) =
        match v with
            | Int x -> sprintf "%d" x
            | Float x -> sprintf "%f" x
            | String x -> sprintf "%s" x

    let rec visitExpressionTree (expressionTree:ScalarExpression) =
        match expressionTree with
            | Atom(a)  -> visitValue a
            | Binary(NameScope,e1,e2) -> sprintf "%s.%s" (visitExpressionTree e1) (visitExpressionTree e2)
            | Binary(o,e1,e2) -> sprintf "%s %A %s" (visitExpressionTree e1) o (visitExpressionTree e2)
            | _ -> "goo"
    
    let rec visitQuery (sql:SqlStatement) =
        sql.Columns |> Seq.map(fun x -> 
                match x with 
                | Expression(s,v) -> sprintf "%s as %s,\n" (visitExpressionTree s) (visitValue v)
                | ColumnSubquery(s) -> sprintf "(%A\n) as %s,\n" (visitQuery (fst s)) (visitValue (snd s))
        ) |> Seq.reduce (+)

    [<EntryPoint>]
        let main argv = 
            let sql = stripSpExecuteSql test03
            printfn "Before: %A\n\n" sql
            let lexbuf = LexBuffer<_>.FromString sql
            let sqlTree =
                try
                    Parser.start Lexer.tokenize lexbuf 
                with
                    | ex -> printfn "Parse error started at %A" (sql.Substring(lexbuf.StartPos.AbsoluteOffset)); raise(Exception("Parse error"))
            printfn "%A\n\n" sqlTree
            printfn "%A\n\n" (visitQuery sqlTree)
            Console.WriteLine("(press any key)")   
            Console.ReadKey(true) |> ignore
            0

