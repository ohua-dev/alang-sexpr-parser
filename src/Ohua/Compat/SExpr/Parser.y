{
-- |
-- Module      : $Header$
-- Description : Parser for ALang S-Expressions
-- Copyright   : (c) Sebastian Ertel, Justus Adam 2017. All Rights Reserved.
-- License     : EPL-1.0
-- Maintainer  : sebastian.ertel@gmail.com, dev@justus.science
-- Stability   : experimental
-- Portability : portable

-- This source code is licensed under the terms described in the associated LICENSE.TXT file
{-# LANGUAGE OverloadedStrings #-}
module Ohua.Compat.SExpr.Parser where

import Ohua.Compat.SExpr.Lexer
import qualified Data.Text as T
import Ohua.ALang.Lang
import Ohua.Types

}


%name parseExp Exp
%name parseNS NS
%tokentype { Lexeme }
%error { parseError }

%token

    id      { Id $$ }

    let     { KWLet }
    fn      { KWFn }
    defalgo { KWDefalgo }
    require { KWRequire }
    ns      { KWNS }
    run     { KWRun }
    '('     { LParen }
    ')'     { RParen }
    '['     { LBracket }
    ']'     { RBracket }

%%

Exp 
    : '(' Form ')'  { $2 }
    | id            { Var (Local $1) }

Form
    : let '[' Binds ']' Stmts { $3 $5 }
    | fn '[' Params ']' Stmts { $3 $5 }
    | Apply { $1 }

Apply
    : Exp { $1 }
    | Apply Exp { Apply $1 $2 }

Binds
    : Assign Exp Binds { Let $1 $2 . $3 }
    | Assign Exp { Let $1 $2 }

Params 
    : Assign Params { Lambda $1 . $2 }
    | Assign { Lambda $1 }

Stmts
    : Exp Stmts { Let "_" $1 $2 }
    | Exp { $1 }

Assign
    : id { Direct $1 }
    | '[' Ids ']' { Destructure $2 }

Ids : id Ids { $1 : $2 }
    | id { [$1] }

NS  : NSHeader Decls RunDecl    { Namespace $1 $2 (Just $3) }
    | NSHeader Decls            { Namespace $1 $2 Nothing }

NSHeader 
    : '(' ns id ')' { $3 }

Decls 
    : '(' Decl ')' Decls    { $2 : $4 }
    |                       { [] }

Decl
    : defalgo id Params Stmts   { AlgoDecl $2 ($3 $4) }
    | require Requires          { ReqDec $2 }

Requires
    : '[' Require ']' Requires  { $2 : $4 }
    |                           { [] }

Require
    : id '[' ReferList ']'  { Require $1 $3 }
    | id                    { Require $1 [] }

ReferList 
    : id ReferList  { $1 : $2 }
    |               { [] }

RunDecl 
    : '(' run Stmts ')' { $3 }

{

data Namespace = Namespace Binding [Declaration] (Maybe Expression)

data Declaration
    = AlgoDecl Binding Expression
    | ReqDec [Require]

data Require = Require Binding [Binding]

parseError :: [Lexeme] -> a
parseError tokens = error $ "Parse error" ++ show tokens
}
