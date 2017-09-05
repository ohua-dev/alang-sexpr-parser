{
-- |
-- Module      : $Header$
-- Description : Lexer for S-Expressions
-- Copyright   : (c) Sebastian Ertel, Justus Adam 2017. All Rights Reserved.
-- License     : EPL-1.0
-- Maintainer  : sebastian.ertel@gmail.com, dev@justus.science
-- Stability   : experimental

-- This source code is licensed under the terms described in the associated LICENSE.TXT file
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
module Ohua.Compat.SExpr.Lexer (lex, Lexeme(..)) where

import qualified Data.Text as T
import qualified Data.Text.Encoding as L
import qualified Data.ByteString.Lazy.Char8 as BS
import Ohua.Types
import Prelude hiding (lex)
}

%wrapper "basic-bytestring"

$char = [a-zA-Z]
$sym  = [\-\>\<\.\$\*\+\?\~\^\/\=_]
$numerical = [0-9]
$reserved = [@\#:\{\}]
$idchar = [$numerical $sym $char]
$sep = [$white \,]

@id = $idchar+


:-

    "("         { const LParen }
    ")"         { const RParen }
    "["         { const LBracket }
    "]"         { const RBracket }
    "let"       { const KWLet }
    "fn"        { const KWFn }
    "defalgo"   { const KWDefalgo }
    "require"   { const KWRequire }
    "ns"        { const KWNS }
    "if"        { const KWIf }
    @id         { Id . Binding . L.decodeUtf8 . BS.toStrict }
    $sep        ;

    $reserved { \s -> error $ "Reserved symbol: " ++ BS.unpack s }


{

data Lexeme
    = LParen -- ^ @(@
    | RParen -- ^ @)@
    | LBracket -- ^ @[@
    | RBracket -- ^ @]@
    | KWLet -- ^ keyword @let@
    | KWFn  -- ^ keyword @fn@
    | KWDefalgo -- ^ keyword @defalgo@
    | KWRequire -- ^ keyword @require@
    | KWIf -- ^ keyword @if@
    | KWNS -- ^ keyword @ns@ (namespace)
    | Id Binding -- ^ an identifier
    deriving Show


-- | Tokenize a lazy bytestring into lexemes
lex :: BS.ByteString -> [Lexeme]
lex = alexScanTokens

}
