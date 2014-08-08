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

    let stripSpExecuteSql (input:string) =
        if not(input.Trim().StartsWith("exec sp_executesql")) then 
            input.Trim()
        else
            if not(input.Contains("@p__linq__0")) then
                input.Trim().Replace("exec sp_executesql N'","").TrimEnd([|'\''|])
            else
                let stripped = input.Trim().Replace("exec sp_executesql N'","") |> (fun x -> x.Substring(0, x.IndexOf("]',")+1))
                (new Regex(@"@p__linq__[\d]+=[^,]+")).Matches(input) 
                    |> Seq.cast
                    |> Seq.map(fun x-> x:> System.Text.RegularExpressions.Match)
                    |> Seq.map(fun x-> ((x.Value.Split [|'='|]).[0].Trim(), (x.Value.Split [|'='|]).[1].Trim()))
                    //|> Seq.iter(fun x-> printfn "%A" x)
                    |> Seq.fold(fun (acc:string) (k, v) -> acc.Replace(k,v)) stripped
    [<EntryPoint>]
        let main argv = 
            let sql = stripSpExecuteSql test02
            printfn "Before: %A" sql
            //let lexbuf = LexBuffer<_>.FromString sql
            //Parser.start Lexer.tokenize lexbuf |> printfn "%A" 
            Console.WriteLine("(press any key)")   
            Console.ReadKey(true) |> ignore
            0

