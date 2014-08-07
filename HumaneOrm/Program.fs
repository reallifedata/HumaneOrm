namespace HumaneOrm
open System
open Sql
open Lexer
open Microsoft.FSharp.Text.Lexing

module Program =
    //open Sql
    [<EntryPoint>]
        let main argv = 
            let x = "   
                SELECT x, y, z   
                FROM t1   
                LEFT JOIN t2   
                INNER JOIN t3 ON t3.ID = t2.ID   
                WHERE x = 50 AND y = 20   
                ORDER BY x ASC, y DESC, z   
            "   
 
            let lexbuf = LexBuffer<string>.FromString x
            let y = Parser.start Lexer.tokenize lexbuf   
            printfn "%A" y   
 
            Console.WriteLine("(press any key)")   
            Console.ReadKey(true) |> ignore
            0

