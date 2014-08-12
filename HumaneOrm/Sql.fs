namespace HumaneOrm

module Sql =
    type Value =   
        | Int of int  
        | Float of float  
        | String of string  

    type Dir = Asc | Desc   

    type Op = Eq | Gt | Ge | Lt | Le | Add | Sub | Div | Mul | Mod | Pow | Count | NameScope | Like | Escape | And | Or // =, >, >=, <, <=   
 
    type Order = Value * Dir   
 
//    type Where =   
//        | Cond of (Value * Op * Value)   
//        | And of Where * Where   
//        | Or of Where * Where   
 
    type JoinType = Inner | Left | Right | InnerApply | OuterApply | CrossApply
 
    type ScalarExpression =
        | Atom of Value
        | Unary of (Op * ScalarExpression)
        | Binary of (Op * ScalarExpression * ScalarExpression)
        | Trinary of (Op * ScalarExpression * ScalarExpression * ScalarExpression)
    
    type TopDistinct =
        | Distinct
        | Top of int

    type  SqlStatement =   
        { Table : JoinTable;   
            TopDistinct : TopDistinct option; 
            Columns : Column list;   
            Joins : Join list;   
            Where : ScalarExpression option;   
            OrderBy : Order list }
    and Column = 
        | Expression of (ScalarExpression * Value)
        | ColumnSubquery of Subquery
    and JoinTable =
        | Table of (Value * Value)
        | TableSubquery of Subquery
    and Join = JoinTable * JoinType * ScalarExpression option
    and Subquery = SqlStatement * Value