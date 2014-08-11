namespace HumaneOrm

module Sql =
    type Value =   
        | Int of int  
        | Float of float  
        | String of string  

    type Dir = Asc | Desc   

    type Op = Eq | Gt | Ge | Lt | Le | Add | Sub | Div | Mul | Mod | Pow | Count | NameScope   // =, >, >=, <, <=   
 
    type Order = Value * Dir   
 
    type Where =   
        | Cond of (Value * Op * Value)   
        | And of Where * Where   
        | Or of Where * Where   
 
    type JoinType = Inner | Left | Right   
 
    
    type ScalarExpression =
        | Atom of Value
        | Unary of (Op * ScalarExpression)
        | Binary of (Op * ScalarExpression * ScalarExpression)
        | Trinary of (Op * ScalarExpression * ScalarExpression * ScalarExpression)
    
    type TopDistinct =
        | Distinct
        | Top of int

    type  SqlStatement =   
        { Table : string;   
            TopDistinct : TopDistinct option; 
            Columns : Column list;   
            Joins : Join list;   
            Where : Where option;   
            OrderBy : Order list }
    and Column = 
        | Expression of (ScalarExpression * Value)
        | Subquery of (SqlStatement * Value)
    and JoinTable =
        | Table of string
        | Subquery of (SqlStatement * string)
    and Join = JoinTable * JoinType * Where option
