namespace HumaneOrm
open System
open System.Text
open System.Text.RegularExpressions
open Sql
open Lexer
open Microsoft.FSharp.Text.Lexing

module Program =
    let (|Match|_|) pattern input =
        let re = new Regex(pattern)
        let m = re.Match(input) in
        if m.Success then Some ((re.GetGroupNames()
                                    |> Seq.map (fun n -> (n, m.Groups.[n]))
                                    |> Seq.filter (fun (n, g) -> g.Success)
                                    |> Seq.map (fun (n, g) -> (n, g.Value))
                                    |> Map.ofSeq)) else None


    let stripSpExecuteSql (input:string) =
        if not(input.Trim().StartsWith("exec sp_executesql")) then 
            input
        else
            if not(input.Contains("@p__linq__0")) then
                input.Trim().Replace("exec sp_executesql N'","").TrimEnd([|'\''|])
            else
                let back = input.Trim().Replace("exec sp_executesql N'","")

    [<EntryPoint>]
        let main argv = 
            let x = "   
                SELECT x, y, z   
                FROM t1   
                LEFT JOIN t2   
                INNER JOIN t3 ON t3.ID = t2.ID   
                WHERE x = 50 AND y = 20  AND z='bar'
                ORDER BY x ASC, y DESC, z   
            "   
            let x1 = "
            exec sp_executesql N'SELECT 
[GroupBy1].[A1] AS [C1]
FROM ( SELECT 
    COUNT(1) AS [A1]
    FROM [dbo].[Tabulka] AS [Extent1]
    WHERE ([Extent1].[Column] LIKE @p__linq__0 ESCAPE N''~'') AND ([Extent1].[Column] LIKE @p__linq__1 ESCAPE N''~'')
)  AS [GroupBy1]',N'@p__linq__0 nvarchar(4000),@p__linq__1 nvarchar(4000)',@p__linq__0=N'%text1%',@p__linq__1=N'%text2%'
            "
            printfn "%A" (stripSpExecuteSql x1)
            let lexbuf = LexBuffer<_>.FromString x
            let y = Parser.start Lexer.tokenize lexbuf   
            printfn "%A" y   
 
            Console.WriteLine("(press any key)")   
            Console.ReadKey(true) |> ignore
            0

