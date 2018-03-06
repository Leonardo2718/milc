--
-- Copyright (C) 2018 Leonardo Banderali
--
-- License:
--
--     Permission is hereby granted, free of charge, to any person obtaining a copy
--     of this software and associated documentation files (the "Software"), to deal
--     in the Software without restriction, including without limitation the rights
--     to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
--     copies of the Software, and to permit persons to whom the Software is
--     furnished to do so, subject to the following conditions:
--
--     The above copyright notice and this permission notice shall be included in
--     all copies or substantial portions of the Software.
--
--     THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
--     IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
--     FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
--     AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
--     LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
--     OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
--     THE SOFTWARE.
--

{
module MParser (parse) where

import CompilerEnvironment
import MLexer
import MilcAST
}

%name parseM
%lexer { alexwrap } { EOF }
%monad { Alex }
%error { parseError }
%tokentype { Token }
%token
    Var     { Token VAR_T _ }
    Fun     { Token FUN_T _ }
    Data    { Token DATA_T _ }
    Id      { Token (ID_T _) _ }
    Ctor    { Token (CTOR_T _) _ }
    IntVal  { Token (INTVAL_T _) _ }
    RealVal { Token (REALVAL_T _) _ }
    CharVal { Token (CHARVAL_T _) _ }
    BoolVal { Token (BOOLVAL_T _) _ }
    Int     { Token INT_T _ }
    Char    { Token CHAR_T _ }
    Real    { Token REAL_T _ }
    Bool    { Token BOOL_T _ }
    ';'     { Token SEMICOLON_T _ }
    ':'     { Token COLON_T _ }
    ','     { Token COMMA_T _ }
    '('     { Token LPAREN_T _ }
    ')'     { Token RPAREN_T _ }
    '['     { Token LBRACK_T _ }
    ']'     { Token RBRACK_T _ }
    '{'     { Token LBRACE_T _ }
    '}'     { Token RBRACE_T _ }
    '|'     { Token PIPE_T _ }
    '='     { Token EQ_T _ }
    '<'     { Token LT_T _ }
    '>'     { Token GT_T _ }
    '=<'    { Token LE_T _ }
    '>='    { Token GE_T _ }
    '+'     { Token ADD_T _ }
    '-'     { Token SUB_T _ }
    '*'     { Token MUL_T _ }
    '/'     { Token DIV_T _ }
    Or      { Token OR_T _ }
    And     { Token AND_T _ }
    Not     { Token NOT_T _ }
    Size    { Token SIZE_T _ }
    Float   { Token FLOAT_T _ }
    Floor   { Token FLOOR_T _ }
    Ceil    { Token CEIL_T _ }
    ':='    { Token ASSIGN_T _ }
    '=>'    { Token ARROW_T _ }
    Begin   { Token BEGIN_T _ }
    End     { Token END_T _ }
    Return  { Token RETURN_T _ }
    If      { Token IF_T _ }
    Then    { Token THEN_T _ }
    Else    { Token ELSE_T _ }
    While   { Token WHILE_T _ }
    Do      { Token DO_T _ }
    Read    { Token READ_T _ }
    Print   { Token PRINT_T _ }
    Case    { Token CASE_T _ }
    Of      { Token OF_T _ }

%%

program :: { AST }
    : block {% do env <- getLexerEnvironment; return (AST (lexSourceFile env) $1) }

block :: { MScope  }
    : declarations program_body { MScope $1 $2 }

declarations :: { [WithPos MDeclaration] }
    : declaration ';' declarations  { $1:$3 }
    | {- empty -}                   { [] }

declaration :: { WithPos MDeclaration }
    : Var var_specs ':' type
        { WithPos (Vars $2 $4) (tokenPos $1) }
    | Fun Id param_list ':' type '{' fun_block '}'
        { WithPos (Fun (emitId $2) $5 $3 $7) (tokenPos $1) }
    | Data Id '=' ctor_declarations
        { WithPos (Data (emitId $2) $4) (tokenPos $1) }

program_body :: { [WithPos MStatement] }
    : Begin prog_stmts End  { $2 }
    | prog_stmts            { $1 }

-- variable declarations
var_specs :: { [WithPos DeclSpec] }
    : var_spec more_var_specs   { $1:$2 }

more_var_specs :: { [WithPos DeclSpec] }
    : ',' var_spec more_var_specs   { $2:$3 }
    | {- empty -}                   { [] }

var_spec ::  { WithPos DeclSpec }
    : Id array_dimensions   { emitIdWith (\s -> DeclSpec s $2) $1 }

array_dimensions:: { [WithPos MExpression] }
    : '[' expr ']' array_dimensions { $2:$4 }
    | {- empty -}                   { [] }

-- function declarations
fun_block :: { [WithPos MDeclaration] }
    : declarations fun_body { $1 }

param_list :: { [WithPos MParamDecl] }
    : '(' parameters ')'    { $2 }

parameters :: { [WithPos MParamDecl] }
    : basic_declaration more_parameters { $1:$2 }
    | {- empty -}                       { [] }

more_parameters :: { [WithPos MParamDecl] }
    : ',' basic_declaration more_parameters { $2:$3 }
    | {- empty -}                           { [] }

basic_declaration :: { WithPos MParamDecl }
    : Id basic_array_dimensions ':' type    { emitIdWith (\s -> MParamDecl s $2 $4) $1}

basic_array_dimensions :: { Int }
    : '[' ']' basic_array_dimensions    { 1 + $3 }
    | {- empty -}                       { 0 }

fun_body :: {}
    : Begin prog_stmts Return expr ';' End  {}
    | prog_stmts Return expr ';'            {}

-- data type declarations
ctor_declarations :: { [WithPos MConstructor] }
    : ctor_decl more_ctor_decl  { $1:$2 }
    | {- empty -}               { [] }

more_ctor_decl :: { [WithPos MConstructor] }
    : '|' ctor_decl more_ctor_decl  { $2:$3 }
    | {- empty -}                   { [] }

ctor_decl :: { WithPos MConstructor }
    : Ctor Of type_list { WithPos (MCtor (token_ctor $1) $3) (tokenPos $1) }
    | Ctor              { WithPos (MCtor (token_ctor $1) []) (tokenPos $1) }

type_list :: { [WithPos MType] }
    : type more_type    { $1:$2 }

more_type :: { [WithPos MType] }
    : '*' type more_type    { $2:$3 }
    | {- empty -}           { [] }

-- data types
type :: { WithPos MType }
    : Int   { emitType $1 }
    | Real  { emitType $1 }
    | Char  { emitType $1 }
    | Bool  { emitType $1 }
    | Id    { emitType $1 }

-- statements
prog_stmts :: { [WithPos MStatement] }
    : prog_stmt ';' prog_stmts  { $1:$3 }
    | {- empty -}               { [] }

prog_stmt :: { WithPos MStatement }
    : If expr Then prog_stmt Else prog_stmt { WithPos (IfThenElse $2 $4 $6) (tokenPos $1) }
    | While expr Do prog_stmt               { WithPos (WhileDo $2 $4) (tokenPos $1) }
    | Case expr Of '{' case_list '}'        { WithPos (CaseOf $2 $5) (tokenPos $1) }
    | location ':=' expr                    { WithPos (Assign $1 $3) (tokenPos $2) }
    | Read location                         { WithPos (MRead $2) (tokenPos $1) }
    | Print expr                            { WithPos (MPrint $2) (tokenPos $1) }
    | '{' block '}'                         { WithEndPos (CodeBlock $2) (tokenPos $1) (tokenPos $3) }

location :: { WithPos MIdentifier }
    : Id array_dimensions   { emitId $1 }

case_list :: { [WithPos MCase] }
    : case more_case    { $1:$2 }

more_case :: { [WithPos MCase] }
    : '|' case more_case    { $2:$3 }
    | {- empty -}           { [] }

case :: { WithPos MCase }
    : Ctor var_list '=>' prog_stmt  { WithPos (MCase (emitIdWithNoP (\s -> MDtor s $2) $1) $4) (tokenPos $1) }

var_list :: { [WithPos MIdentifier] }
    : '(' var_list1 ')' { $2 }
    | {- empty -}       { [] }

var_list1 :: { [WithPos MIdentifier] }
    : Id more_var_list1 { emitId $1 : $2 }

more_var_list1 :: { [WithPos MIdentifier] }
    : ',' Id more_var_list1 { emitId $2 : $3 }
    | {- empty -}           { [] }

-- expressions
expr :: { WithPos MExpression }
    : expr Or bint_term { emitBinOp $2 $1 $3 }
    | bint_term         { $1 }

bint_term :: { WithPos MExpression }
    : bint_term And bint_factor { emitBinOp $2 $1 $3 }
    | bint_factor               { $1 }

bint_factor :: { WithPos MExpression }
    : Not bint_factor               { WithPos (MUnaryOp MNot $2) (tokenPos $1) }
    | int_expr compare_op int_expr  { emitBinOp $2 $1 $3 }
    | int_expr                      { $1 }

compare_op :: { Token }
    : '='   { $1 }
    | '<'   { $1 }
    | '>'   { $1 }
    | '=<'  { $1 }
    | '>='  { $1 }

int_expr :: { WithPos MExpression }
    : int_expr add_op int_term  { emitBinOp $2 $1 $3}
    | int_term                  { $1 }

add_op :: { Token }
    : '+'   { $1 }
    | '-'   { $1 }

int_term :: { WithPos MExpression }
    : int_term mul_op int_factor    { emitBinOp $2 $1 $3 }
    | int_factor                    { $1 }

mul_op :: { Token }
    : '*'   { $1 }
    | '/'   { $1 }

int_factor :: { WithPos MExpression }
    : '(' expr ')'                          { $2 }
    | Size '(' Id basic_array_dimensions ')'{ emitIdIn (\i -> MSize i $4) $3 }
    | Float '(' expr ')'                    { emitUniOp MFloat $1 $3 }
    | Floor '(' expr ')'                    { emitUniOp MFloor $1 $3 }
    | Ceil '(' expr ')'                     { emitUniOp MCeil $1 $3 }
    | Id array_dimensions                   { emitIdIn (\n -> MVar n $2) $1 }
    | Id fun_argument_list                  { emitIdIn (\n -> MCall n $2) $1 }
    | Ctor ctor_argument_list               { emitIdIn (\n -> MCtorVal n $2) $1 }
    | IntVal                                { emitConst IntConst token_intval $1 }
    | RealVal                               { emitConst RealConst token_realval $1 }
    | CharVal                               { emitConst CharConst token_charval $1 }
    | BoolVal                               { emitConst BoolConst token_boolval $1 }
    | '-' int_factor                        { emitUniOp MNeg $1 $2 }

fun_argument_list :: { [WithPos MExpression] }
    : '(' arguments ')' { $2 }

ctor_argument_list :: { [WithPos MExpression] }
    : fun_argument_list { $1 }
    | {- empty -}       { [] }

arguments :: { [WithPos MExpression] }
    : expr more_arguments   { $1:$2 }
    | {- empty -}           { [] }

more_arguments :: { [WithPos MExpression] }
    : ',' expr more_arguments   { $2:$3 }
    | {- empty -}               { [] }

{

emitType :: Token -> WithPos MType
emitType t = WithPos (emitTypeNoP t) (tokenPos t)

emitTypeNoP :: Token -> MType
emitTypeNoP t = case t of
    Token INT_T _       -> Int
    Token REAL_T _      -> Real
    Token CHAR_T _      -> Char
    Token BOOL_T _      -> Bool
    Token (ID_T name) _ -> emitIdWithNoP UserType t

emitConst :: (a -> MConstant) -> (Token -> a) -> Token -> WithPos MExpression
emitConst asConst getVal t = WithPos (MConst . asConst . getVal $ t) (tokenPos t)

emitIdWith :: (String -> a) -> Token -> WithPos a
emitIdWith c t = WithPos (emitIdWithNoP c t) (tokenPos t)

emitIdWithNoP :: (String -> a) -> Token -> a
emitIdWithNoP c = c . token_idname

emitId :: Token -> WithPos MIdentifier
emitId = emitIdWith MIdName

emitIdNoP :: Token -> MIdentifier
emitIdNoP = emitIdWithNoP MIdName

emitIdIn :: (MIdentifier -> a) -> Token -> WithPos a
emitIdIn c t = WithPos (emitIdInNoP c t) (tokenPos t)

emitIdInNoP :: (MIdentifier -> a) -> Token -> a
emitIdInNoP c = c . emitIdNoP

emitBinOp :: Token -> WithPos MExpression -> WithPos MExpression -> WithPos MExpression
emitBinOp tok lhs rhs = WithPos (MBinaryOp op lhs rhs) (tokenPos tok) where
    op = case tok of
        Token ADD_T _   -> MAdd
        Token SUB_T _   -> MSub
        Token MUL_T _   -> MMul
        Token DIV_T _   -> MDiv
        Token OR_T _    -> MOr
        Token AND_T _   -> MAnd
        Token EQ_T _    -> MEqual
        Token LT_T _    -> MLessThan
        Token LE_T _    -> MLessEqual
        Token GT_T _    -> MGreaterThan
        Token GE_T _    -> MGreaterEqual

emitUniOp :: MUnaryOp -> Token -> WithPos MExpression -> WithPos MExpression
emitUniOp op tok expr = WithPos (MUnaryOp op expr) (tokenPos tok)

alexwrap :: (Token -> Alex a) -> Alex a
alexwrap = (scanToken >>=)

userParseError :: String -> Token -> Alex a
userParseError msg t = do
    env <- getLexerEnvironment
    let errMsg = case t of
            EOF -> concat   [ "Parse error at EOF:\n"
                            , if msg == "" then "Expecting more tokens after:\n"
                                           else (msg ++ "\n")
                            , "| ", (last . lines) (lexSource env)
                            ]
            _   -> concat   [ "Parse error at ", showAlexPos pos, ":\n"
                            , "Unexpected token: ", show (tokenType t) ,"\n"
                            , if msg == "" then "" else (msg ++ "\n")
                            , showErrorLocation source line column
                            ] where
                                pos = tokenPos t
                                AlexPn _ line column = pos
                                source = lexSource env
    alexError errMsg

parseError :: Token -> Alex a
parseError = userParseError ""

parse :: Monad m => LexerEnvironment -> CompilerMonadT AST m
parse env = do
    logMsgLn "=== Running parser ==="
    ast <- runAlexCompiler env parseM
    logMsgLn "Parsing successful"
    logMsgLn "Generated AST:"
    logTree ast
    return ast

}
