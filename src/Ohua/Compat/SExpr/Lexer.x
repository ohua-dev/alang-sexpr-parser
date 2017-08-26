{
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
module Ohua.Compat.SExpr.Lexer where

import qualified Data.Text as T
import qualified Data.Text.Encoding as L
import qualified Data.ByteString.Lazy.Char8 as BS
import Ohua.Types
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

    "("     { const LParen }
    ")"     { const RParen }
    "["     { const LBracket }
    "]"     { const RBracket }
    "let"   { const KWLet }
    "fn"    { const KWFn }
    @id     { Id . Binding . L.decodeUtf8 . BS.toStrict }
    $sep    ;

    $reserved { \s -> error $ "Reserved symbol: " ++ BS.unpack s }


{

data Lexeme
    = LParen
    | RParen
    | LBracket
    | RBracket
    | KWLet
    | KWFn
    | Id Binding
    deriving Show


lex = alexScanTokens

}
