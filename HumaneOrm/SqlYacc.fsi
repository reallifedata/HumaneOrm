// Signature file for parser generated by fsyacc
module Parser
type token = 
  | EOF
  | ASC
  | DESC
  | SELECT
  | FROM
  | WHERE
  | ORDER
  | BY
  | JOIN
  | INNER
  | LEFT
  | RIGHT
  | ON
  | EQ
  | LT
  | LE
  | GT
  | GE
  | COMMA
  | AND
  | OR
  | FLOAT of (float)
  | INT of (int)
  | ID of (string)
type tokenId = 
    | TOKEN_EOF
    | TOKEN_ASC
    | TOKEN_DESC
    | TOKEN_SELECT
    | TOKEN_FROM
    | TOKEN_WHERE
    | TOKEN_ORDER
    | TOKEN_BY
    | TOKEN_JOIN
    | TOKEN_INNER
    | TOKEN_LEFT
    | TOKEN_RIGHT
    | TOKEN_ON
    | TOKEN_EQ
    | TOKEN_LT
    | TOKEN_LE
    | TOKEN_GT
    | TOKEN_GE
    | TOKEN_COMMA
    | TOKEN_AND
    | TOKEN_OR
    | TOKEN_FLOAT
    | TOKEN_INT
    | TOKEN_ID
    | TOKEN_end_of_input
    | TOKEN_error
type nonTerminalId = 
    | NONTERM__startstart
    | NONTERM_start
    | NONTERM_columnList
    | NONTERM_joinList
    | NONTERM_joinClause
    | NONTERM_joinOnClause
    | NONTERM_conditionList
    | NONTERM_whereClause
    | NONTERM_op
    | NONTERM_value
    | NONTERM_orderByClause
    | NONTERM_orderByList
    | NONTERM_orderBy
/// This function maps integers indexes to symbolic token ids
val tagOfToken: token -> int

/// This function maps integers indexes to symbolic token ids
val tokenTagToTokenId: int -> tokenId

/// This function maps production indexes returned in syntax errors to strings representing the non terminal that would be produced by that production
val prodIdxToNonTerminal: int -> nonTerminalId

/// This function gets the name of a token as a string
val token_to_string: token -> string
val start : (Microsoft.FSharp.Text.Lexing.LexBuffer<'cty> -> token) -> Microsoft.FSharp.Text.Lexing.LexBuffer<'cty> -> (Sql.sqlStatement) 
