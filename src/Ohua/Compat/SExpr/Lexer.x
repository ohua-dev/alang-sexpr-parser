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
{-# OPTIONS_GHC -funbox-strict-fields -fno-warn-unused-imports #-}
module Ohua.Compat.SExpr.Lexer (tokenize, Lexeme(..)) where

import Ohua.Prelude hiding (undefined)

import qualified Data.ByteString.Lazy.Char8 as BS

import Prelude (undefined, read)
}

%wrapper "basic-bytestring"

$char = [a-zA-Z]
$sym  = [\-\>\<\$\*\+\?\~\^\=_]
$num_not_zero = [1-9]
$numerical = [$num_not_zero 0]
$reserved = [@\#:\{\}]
$idchar = [$numerical $sym $char]
$sep = [$white \,]

@id = $idchar+
@ns = @id (\. @id)*
@number = 0 | $num_not_zero $numerical*


:-

    "("             { const LParen }
    ")"             { const RParen }
    "["             { const LBracket }
    "]"             { const RBracket }
    "let"           { const KWLet }
    "fn"            { const KWFn }
    "defalgo"       { const KWDefalgo }
    "require"       { const KWRequire }
    "algo"          { const KWAlgo }
    "sf"            { const KWSf }
    "ns"            { const KWNS }
    "if"            { const KWIf }
    "nil"           { const KWNil }
    "with"          { const KWWith }
    "-"? @number    { Number . read . BS.unpack }
    @id             { UnqualId . convertId }
    @ns\/@id        { QualId . mkQualId }
    @ns             { NSId . mkNSRef }
    $sep            ;

    $reserved       { \s -> error $ "Reserved symbol: " <> decodeUtf8 s }


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
    | KWSf
    | KWAlgo
    | KWIf -- ^ keyword @if@
    | KWNS -- ^ keyword @ns@ (namespace)
    | KWNil
    | KWWith
    | Number Integer
    | UnqualId Binding -- ^ an identifier
    | QualId QualifiedBinding
    | NSId NSRef -- ^ an identifier for a namespace
    deriving Show


convertId :: ByteString.ByteString -> Binding
convertId = makeThrow . decodeUtf8


mkQualId :: BS.ByteString -> QualifiedBinding
mkQualId str = QualifiedBinding (mkNSRef nsstr) (convertId name0)
  where
    (nsstr, name') = BS.break (== '/') str
    name0 = BS.tail name'


mkNSRef :: BS.ByteString -> NSRef
mkNSRef = makeThrow . map convertId . BS.split '.'


-- | Tokenize a lazy bytestring into lexemes
tokenize :: BS.ByteString -> [Lexeme]
tokenize = alexScanTokens

}
