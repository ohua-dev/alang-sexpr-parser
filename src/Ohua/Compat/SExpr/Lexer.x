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
module Ohua.Compat.SExpr.Lexer (tokenize, Lexeme(..)) where

import qualified Data.Text as T
import qualified Data.Text.Encoding as L
import qualified Data.ByteString.Lazy.Char8 as BS
import Ohua.Types
import Prelude hiding (lex)
}

%wrapper "basic-bytestring"

$char = [a-zA-Z]
$sym  = [\-\>\<\$\*\+\?\~\^\=_]
$numerical = [0-9]
$reserved = [@\#:\{\}]
$idchar = [$numerical $sym $char]
$sep = [$white \,]

@id = $idchar+
@ns = @id (\. @id)*


:-

    "("             { const LParen }
    ")"             { const RParen }
    "["             { const LBracket }
    "]"             { const RBracket }
    "let"           { const KWLet }
    "fn"            { const KWFn }
    "defalgo"       { const KWDefalgo }
    "require-sf"    { const KWRequireSf }
    "require-algo"  { const KWRequireAlgo }
    "ns"            { const KWNS }
    "if"            { const KWIf }
    @id             { UnqualId . convertId }
    @ns\/@id        { QualId . mkQualId }
    @ns             { NSId . mkNSRef }
    $sep            ;

    $reserved       { \s -> error $ "Reserved symbol: " ++ BS.unpack s }


{

data Lexeme
    = LParen -- ^ @(@
    | RParen -- ^ @)@
    | LBracket -- ^ @[@
    | RBracket -- ^ @]@
    | KWLet -- ^ keyword @let@
    | KWFn  -- ^ keyword @fn@
    | KWDefalgo -- ^ keyword @defalgo@
    | KWRequireSf -- ^ keyword @require-sf@
    | KWRequireAlgo -- ^ keyword @require-algo@
    | KWIf -- ^ keyword @if@
    | KWNS -- ^ keyword @ns@ (namespace)
    | UnqualId Binding -- ^ an identifier
    | QualId QualifiedBinding
    | NSId NSRef -- ^ an identifier for a namespace
    deriving Show


convertId :: ByteString.ByteString -> Binding
convertId = Binding . L.decodeUtf8 . BS.toStrict


mkQualId :: BS.ByteString -> QualifiedBinding
mkQualId str = QualifiedBinding (mkNSRef nsstr) (convertId name)
  where
    (nsstr, name') = BS.break (== '/') str
    name = BS.tail name'


mkNSRef :: BS.ByteString -> NSRef
mkNSRef = nsRefFromList . map Binding . T.split (== '.') . L.decodeUtf8 . BS.toStrict


-- | Tokenize a lazy bytestring into lexemes
tokenize :: BS.ByteString -> [Lexeme]
tokenize = alexScanTokens

}
