{
{-# LANGUAGE OverloadedStrings #-}
module Ohua.Compat.SExpr.Parser where

import Ohua.Compat.SExpr.Lexer
import qualified Data.Text as T
import Ohua.ALang.Lang
import Ohua.Types

}


%name parse
%tokentype { Lexeme }
%error { parseError }

%token

    id      { Id $$ }

    let     { KWLet }
    fn      { KWFn }
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

{
parseError :: [Lexeme] -> a
parseError tokens = error $ "Parse error" ++ show tokens
}
