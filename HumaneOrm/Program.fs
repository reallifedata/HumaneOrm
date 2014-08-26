namespace HumaneOrm
open System
open System.Text
open System.Text.RegularExpressions
open Sql
open Lexer
open StandardVisitor
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
            ,count( select a from b inner join c on a.a = b.b )
            ,cast(x.x as foo)
            ,cast(x.x as datetime2(7))
            ,(select a from baz) as SubqueryTest
            ,(select b from foobar) as [SubqueryTestWithBracket]
        from
            (select distinct bazz from foo) as theTable
            outer apply (select [a] from bar where 1 = 1) as table2
            right outer join Table3 ON a LIKE '%b%'
    "
    let test04="
    exec sp_executesql N'SELECT 
[Element1].[Id] AS [Id], 
[Element1].[PortalUserId] AS [PortalUserId], 
[Element1].[Email] AS [Email], 
[Element1].[FirstName] AS [FirstName], 
[Element1].[LastName] AS [LastName], 
[Element1].[CrmAccountNumber] AS [CrmAccountNumber], 
[Element1].[PortalUserNotificationTypeId] AS [PortalUserNotificationTypeId], 
[Element1].[SmsTextNumber] AS [SmsTextNumber], 
[Element1].[IsEmailEnabled] AS [IsEmailEnabled], 
[Element1].[IsSmsTextEnabled] AS [IsSmsTextEnabled], 
[Element1].[DistanceRadius] AS [DistanceRadius]
FROM   (SELECT 
	[Extent2].[PortalUserId] AS [PortalUserId]
	FROM  [dbo].[ReferralAccount] AS [Extent1]
	INNER JOIN [dbo].[ReferralInquiryFollow] AS [Extent2] ON  EXISTS (SELECT 
		1 AS [C1]
		FROM    ( SELECT 1 AS X ) AS [SingleRowTable1]
		LEFT OUTER JOIN  (SELECT 
			[Extent3].[Id] AS [Id], 
			[Extent3].[AccountId] AS [AccountId]
			FROM [dbo].[PortalUser] AS [Extent3]
			WHERE [Extent2].[PortalUserId] = [Extent3].[Id] ) AS [Project1] ON 1 = 1
		LEFT OUTER JOIN  (SELECT 
			[Extent4].[Id] AS [Id], 
			[Extent4].[AccountId] AS [AccountId]
			FROM [dbo].[PortalUser] AS [Extent4]
			WHERE [Extent2].[PortalUserId] = [Extent4].[Id] ) AS [Project2] ON 1 = 1
		WHERE ([Extent1].[AccountId] = [Project1].[AccountId]) OR (([Extent1].[AccountId] IS NULL) AND ([Project2].[AccountId] IS NULL))
	)
	WHERE ( CAST( [Extent1].[Id] AS bigint) = @p__linq__0) AND ([Extent2].[ReferralInquiryId] = @p__linq__1) ) AS [Project4]
OUTER APPLY  (SELECT TOP (1) 
	[Extent5].[Id] AS [Id], 
	[Extent5].[PortalUserId] AS [PortalUserId], 
	[Extent5].[Email] AS [Email], 
	[Extent5].[FirstName] AS [FirstName], 
	[Extent5].[LastName] AS [LastName], 
	[Extent5].[CrmAccountNumber] AS [CrmAccountNumber], 
	[Extent5].[PortalUserNotificationTypeId] AS [PortalUserNotificationTypeId], 
	[Extent5].[SmsTextNumber] AS [SmsTextNumber], 
	[Extent5].[IsEmailEnabled] AS [IsEmailEnabled], 
	[Extent5].[IsSmsTextEnabled] AS [IsSmsTextEnabled], 
	[Extent5].[DistanceRadius] AS [DistanceRadius]
	FROM [dbo].[PortalUserNotificationPreferencesView] AS [Extent5]
	WHERE ([Project4].[PortalUserId] = [Extent5].[PortalUserId]) AND ( CAST( [Extent5].[PortalUserNotificationTypeId] AS int) = @p__linq__2) ) AS [Element1]',N'@p__linq__0 bigint,@p__linq__1 bigint,@p__linq__2 int',@p__linq__0=33,@p__linq__1=501,@p__linq__2=1
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


    [<EntryPoint>]
        let main argv = 
            let sql = stripSpExecuteSql test04
            printfn "Before: %A\n\n" sql
            let lexbuf = LexBuffer<_>.FromString sql
            let sqlTree =
                try
                    Parser.start Lexer.tokenize lexbuf 
                with
                    | ex -> printfn "Parse error started at %A" (sql.Substring(lexbuf.StartPos.AbsoluteOffset)); raise(Exception("Parse error"))
            printfn "%A\n\n" sqlTree
            printfn "%A\n\n" (visitSqlQuery sqlTree)
            Console.WriteLine("(press any key)")   
            Console.ReadKey(true) |> ignore
            0

