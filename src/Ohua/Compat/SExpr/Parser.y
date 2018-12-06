{
-- |
-- Module      : $Header$
-- Description : Parser for ALang S-Expressions
-- Copyright   : (c) Sebastian Ertel, Justus Adam 2017. All Rights Reserved.
-- License     : EPL-1.0
-- Maintainer  : sebastian.ertel@gmail.com, dev@justus.science
-- Stability   : experimental

-- This source code is licensed under the terms described in the associated LICENSE.TXT file
{-# LANGUAGE OverloadedStrings, FlexibleContexts, LambdaCase, NamedFieldPuns #-}
module Ohua.Compat.SExpr.Parser
    ( parseNS, parseExp
    , Namespace(..)
    ) where

import Ohua.Prelude

import Ohua.Compat.SExpr.Lexer
import Ohua.Frontend.Lang
import Ohua.Frontend.NS
import qualified Data.HashMap.Strict as HM

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
    number          { Number $$ }

    let             { KWLet }
    fn              { KWFn }
    defalgo         { KWDefalgo }
    nil             { KWNil }
    require         { KWRequire }
    sf              { KWSf }
    algo            { KWAlgo }
    ns              { KWNS }
    if              { KWIf }
    with            { KWWith }
    '('             { LParen }
    ')'             { RParen }
    '['             { LBracket }
    ']'             { RBracket }

%%

many1 (p)
    : p many(p) { $1 :| $2 }

many (p)
    : p many(p)  { $1 : $2 }
    |            { [] }

many_sep1(p, sep)
    : p sep many_sep1(p, sep) { let x :| xs = $3 in $1 :| x:xs }
    | p                       { $1 :| [] }

many_sep(p, sep)
    : many_sep1(p, sep) { toList $1 }
    |                   { [] }

opt(p)
    : p { Just $1 }
    |   { Nothing }

or(a, b)
    : a { Left $1 }
    | b { Right $1 }

form(p)
    : '(' p ')' { $2 }

vec(p)
    : '[' p ']' { $2 }

NsId
    :: { NSRef }
    : id    { makeThrow [$1] :: NSRef }
    | nsid  { $1 }

Exp
    :: { Expr }
    : form(Form)        { $1 }
    | vec(many(Exp))    { TupE $1 }
    | number            { LitE $ NumericLit $1 }
    | nil               { LitE UnitLit }
    | id                { VarE $1 }
    | qualid            { LitE $ FunRefLit $ FunRef $1 Nothing }

Form
    :: { Expr }
    : let vec(Binds) Stmts        { $2 $3 }
    | fn vec(many(Pat)) Stmts     { LamE $2 $3 }
    | if Exp Exp Exp              { IfE $2 $3 $4 }
    | with Exp Exp                { BindE $2 $3 }
    | many1(Exp)                  { let fun :| args = $1 in AppE fun args }

Binds
    :: { Expr -> Expr }
    : Pat Exp Binds  { LetE $1 $2 . $3 }
    | Pat Exp        { LetE $1 $2 }

Stmts
    :: { Expr }
    : many(Exp) { case $1 of
                      [] -> LitE UnitLit
                      x:xs -> let safeL = x :| xs in foldr1 StmtE safeL }

Pat
    :: { Pat }
    : id                   { VarP $1 }
    | nil                  { UnitP }
    | vec(many(Pat))       { TupP $1 }

NS  :: { (NSRef, [Decl]) }
    : form(NSHeader) many(form(Decl)) { ($1, $2) }

NSHeader
    :: { NSRef }
    : ns NsId { $2 }

Decl
    :: { Decl }
    : defalgo id vec(many(Pat)) Stmts       { AlgoDecl $ Algo { algoName = $2, algoCode = LamE $3 $4 } }
    | require ReqType vec(many(Require))    { ImportDecl $ map (\i -> i { isAlgo = $2 }) $3 }

ReqType
    :: { Bool }
    : sf   { False }
    | algo { True }


Require
    :: { Import }
    : NsId opt(vec(many(id))) { Import { nsRef = $1, bindings = fromMaybe [] $2 } }

{

data Decl
    = ImportDecl [Import]
    | AlgoDecl Algo
data Import = Import
    { isAlgo :: Bool
    , nsRef :: NSRef
    , bindings :: [Binding]
    }
data Algo = Algo
    { algoName :: Binding
    , algoCode :: Expr
    }

-- | Parse a stream of tokens into a simple ALang expression
parseExp :: [Lexeme] -> Expr
parseExp = parseExpH

parseError :: [Lexeme] -> a
parseError tokens = error $ "Parse error " <> show tokens

partitionDecls :: [Decl] -> ([Import], [Algo])
partitionDecls = flip foldr' ([],[]) . flip $ \(xs,ys) -> \case
    ImportDecl i -> (i<>xs, ys)
    AlgoDecl a -> (xs, a:ys)

-- | Parse a stream of tokens into a namespace
parseNS :: [Lexeme] -> Namespace Expr
parseNS = f . parseNSRaw
  where
    f (name, decls0) = (emptyNamespace name :: Namespace Expr)
      & algoImports .~ map importToTuple algoRequires
      & sfImports .~ map importToTuple sfRequires
      & decls .~ algos
      where
        (requires, algoList) = partitionDecls decls0
        sfRequires = filter (not . isAlgo) requires
        algoRequires = filter isAlgo requires
        importToTuple Import{nsRef, bindings} = (nsRef, bindings)
        algos = HM.fromList $ map (\Algo{algoName, algoCode} -> (algoName, algoCode)) algoList -- ignores algos which are defined twice
}
