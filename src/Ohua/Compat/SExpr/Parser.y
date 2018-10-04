{
-- |
-- Module      : $Header$
-- Description : Parser for ALang S-Expressions
-- Copyright   : (c) Sebastian Ertel, Justus Adam 2017. All Rights Reserved.
-- License     : EPL-1.0
-- Maintainer  : sebastian.ertel@gmail.com, dev@justus.science
-- Stability   : experimental

-- This source code is licensed under the terms described in the associated LICENSE.TXT file
{-# LANGUAGE OverloadedStrings #-}
module Ohua.Compat.SExpr.Parser
    ( parseNS, parseExp
    , Namespace(..)
    ) where

import Ohua.Prelude

import Ohua.Compat.SExpr.Lexer
import Ohua.ALang.Lang
import Ohua.ALang.NS
import qualified Data.HashMap.Strict as HM
import qualified Ohua.ParseTools.Refs as Refs

import Prelude ((!!))

}


%name parseExpH Exp
%name parseNSRaw NS
%tokentype { Lexeme }
%error { parseError }

%token

    id              { UnqualId $$ }
    qualid          { QualId $$ }
    nsid            { NSId $$ }

    let             { KWLet }
    fn              { KWFn }
    defalgo         { KWDefalgo }
    "require-sf"    { KWRequireSf }
    "require-algo"  { KWRequireAlgo }
    ns              { KWNS }
    if              { KWIf }
    '('             { LParen }
    ')'             { RParen }
    '['             { LBracket }
    ']'             { RBracket }

%%

SomeId
    : id     { Unqual $1 }
    | qualid { Qual $1 }

NsId
    : id    { makeThrow [$1] :: NSRef }
    | nsid  { $1 }

Exp
    : '(' Form ')'  { $2 }
    | SomeId        { Var $1 }

Form
    : let '[' Binds ']' Stmts { $3 $5 }
    | fn '[' Params ']' Stmts { $3 $5 }
    | if Exp Exp Exp          { Refs.ifBuiltin `Apply` $2 `Apply` ignoreArgLambda $3 `Apply` ignoreArgLambda $4 }
    | Apply                   { $1 }

Apply
    : Exp { $1 }
    | Apply Exp { Apply $1 $2 }

Binds
    : Assign Exp Binds  { Let $1 $2 . $3 }
    | Assign Exp        { Let $1 $2 }

Params
    : Assign Params { Lambda $1 . $2 }
    | Assign        { Lambda $1 }

Stmts
    : Exp Stmts { ignoreArgLet $1 $2 }
    | Exp       { $1 }

Assign
    : id            { Direct $1 }
    | '[' Ids ']'   { Destructure $2 }

Ids : id Ids    { $1 : $2 }
    | id        { [$1] }

NS  : NSHeader Decls { ($1, $2) }

NSHeader
    : '(' ns NsId ')' { $3 }

Decls
    : '(' Decl ')' Decls    { $2 : $4 }
    |                       { [] }

Decl
    : defalgo id '[' Params ']' Stmts   { Right ($2, $4 $6) }
    | "require-sf" Requires             { Left (Left $2) }
    | "require-algo" Requires           { Left (Right $2) }

Requires
    : '[' Require ']' Requires  { $2 : $4 }
    |                           { [] }

Require
    : NsId '[' ReferList ']'  { ($1, $3) }
    | NsId                    { ($1, []) }

ReferList
    : id ReferList  { $1 : $2 }
    |               { [] }

{

ignoreArgLambda = Lambda (Direct "_")
ignoreArgLet = Let (Direct "_")


-- | Parse a stream of tokens into a simple ALang expression
parseExp :: [Lexeme] -> Expr SomeBinding
parseExp = parseExpH

parseError :: [Lexeme] -> a
parseError tokens = error $ "Parse error " <> show tokens


-- | Parse a stream of tokens into a namespace
parseNS :: [Lexeme] -> Namespace (Expr SomeBinding)
parseNS = f . parseNSRaw
  where
    f (name, decls) = Namespace name (concat algoRequires) (concat sfRequires) algos
      where
        (requires, algoList) = partitionEithers decls
        (sfRequires, algoRequires) = partitionEithers requires
        algos = HM.fromList algoList -- ignores algos which are defined twice
}
