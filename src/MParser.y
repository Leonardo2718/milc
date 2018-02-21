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
    Of      { Token OF_T _ }
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
    '*'     { Token MUL_T _ }

%%

program :: { AST }
    : block {% do env <- getLexerEnvironment; return (AST (lexSourceFile env) $1) }

block :: { Block  }
    : declarations { CodeBlock $1 }

declarations :: { [WithPos MDeclaration] }
    : declaration ';' declarations  { $1:$3 }
    | {- empty -}                   { [] }

declaration :: { WithPos MDeclaration }
    : Var var_specs ':' type    { WithPos
                                    (Vars $2 $4)
                                    (tokenPos $1) }
    | Fun Id param_list ':' type '{' fun_block '}'  { WithPos
                                                        ( Fun
                                                          (WithPos (MIdName (token_idname $2)) (tokenPos $2) )
                                                          $5
                                                          $3
                                                          $7
                                                        )
                                                        (tokenPos $1) }
    | Data Id '=' ctor_declarations { WithPos
                                      ( Data
                                        (WithPos (MIdName (token_idname $2)) (tokenPos $2) )
                                        $4
                                      )
                                      (tokenPos $1)
                                    }

-- variable declarations
var_specs :: { [WithPos DeclSpec] }
    : var_spec more_var_specs   { $1:$2 }

more_var_specs :: { [WithPos DeclSpec] }
    : ',' var_spec more_var_specs   { $2:$3 }
    | {- empty -}                   { [] }

var_spec ::  { WithPos DeclSpec }
    : Id array_dimensions   { WithPos (DeclSpec (token_idname $1) $2) (tokenPos $1) }

array_dimensions:: { [WithPos MExpression] }
    : '[' expr ']' array_dimensions { $2:$4 }
    | {- empty -}                   { [] }

-- function declarations
fun_block :: { [WithPos MDeclaration] }
    : declarations  { $1 }

param_list :: { [WithPos MParamDecl] }
    : '(' parameters ')'    { $2 }

parameters :: { [WithPos MParamDecl] }
    : basic_declaration more_parameters { $1:$2 }
    | {- empty -}                       { [] }

more_parameters :: { [WithPos MParamDecl] }
    : ',' basic_declaration more_parameters { $2:$3 }
    | {- empty -}                           { [] }

basic_declaration :: { WithPos MParamDecl }
    : Id basic_array_dimensions ':' type    { WithPos (MParamDecl (token_idname $1) $2 $4) (tokenPos $1) }

basic_array_dimensions :: { Int }
    : '[' ']' basic_array_dimensions    { 1 + $3 }
    | {- empty -}                       { 0 }

-- data type declarations
ctor_declarations :: { [WithPos MConstructor] }
    : ctor_decl more_ctor_decl  { $1:$2 }
    | {- empty -}               { [] }

more_ctor_decl :: { [WithPos MConstructor] }
    : '|' ctor_decl more_ctor_decl  { $2:$3 }
    | {- empty -}                   { [] }

ctor_decl :: { WithPos MConstructor }
    : Ctor Of type_list { WithPos (MCtorT (token_ctor $1) $3) (tokenPos $1) }
    | Ctor              { WithPos (MCtor (token_ctor $1)) (tokenPos $1) }

type_list :: { [WithPos MType] }
    : type more_type    { $1:$2 }

more_type :: { [WithPos MType] }
    : '*' type more_type    { $2:$3 }
    | {- empty -}           { [] }

-- other things
type :: { WithPos MType }
    : Int   { WithPos Int (tokenPos $1) }
    | Char  { WithPos Char (tokenPos $1) }
    | Real  { WithPos Real (tokenPos $1) }
    | Bool  { WithPos Bool (tokenPos $1) }
    | Id    { WithPos (UserType (token_idname $1)) (tokenPos $1) }

expr :: { WithPos MExpression }
    : IntVal    { WithPos (ConstVal (IntConst (token_intval $1))) (tokenPos $1) }
    | RealVal   { WithPos (ConstVal (RealConst (token_realval $1))) (tokenPos $1) }
    | CharVal   { WithPos (ConstVal (CharConst (token_charval $1))) (tokenPos $1) }
    | BoolVal   { WithPos (ConstVal (BoolConst (token_boolval $1))) (tokenPos $1) }
    | Id        { WithPos (Id (token_idname $1)) (tokenPos $1)}

{

alexwrap :: (Token -> Alex a) -> Alex a
alexwrap = (scanToken >>=)

parseError :: Token -> Alex a
parseError t = do
    env <- getLexerEnvironment
    let errMsg = case t of
            EOF -> concat   [ "Parse error at EOF:\n"
                            , "Expecting more tokens after:\n"
                            , "| ", (last . lines) (lexSource env)
                            ]
            _   -> concat   [ "Parse error at ", showAlexPos pos, ":\n"
                            , "Unexpected token: ", show (tokenType t) ,"\n"
                            , showErrorLocation source line column
                            ] where
                                pos = tokenPos t
                                AlexPn _ line column = pos
                                source = lexSource env
    alexError errMsg

parse :: Monad m => LexerEnvironment -> CompilerMonadT AST m
parse env = do
    logMsgLn "=== Running parser ==="
    ast <- runAlexCompiler env parseM
    logMsgLn "Parsing successful"
    logMsgLn "Generated AST:"
    logTree ast
    return ast

}
