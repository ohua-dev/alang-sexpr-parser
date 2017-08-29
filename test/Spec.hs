{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec
import Ohua.Compat.SExpr.Lexer
import Ohua.Compat.SExpr.Parser
import Ohua.ALang.Lang
import Ohua.Types
import Prelude hiding (lex)


lp = parseExp . lex

main :: IO ()
main = hspec $ do
    describe "parser and lexer" $ do
        it "parse an apply" $
            lp "(something a b c)" `shouldBe` ("something" `Apply` "a" `Apply` "b" `Apply` "c")
        it "parses a let" $
            lp "(let [a b] b)" `shouldBe` Let "a" "b" "b"
        it "parses a lambda" $
            lp "(fn [a [b c]] (print a) c)" `shouldBe` (Lambda "a" $ Lambda (Destructure ["b", "c"]) $ Let "_" ("print" `Apply` "a") "c")
        it "parses an identifier with strange symbols" $
            lp "(let [a-b a0] -)" `shouldBe` Let "a-b" "a0" "-"
        it "parses longer let binds" $
            lp "(let [a 0 b 1 c (print 6)] a)" `shouldBe` (Let "a" "0" $ Let "b" "1" $ Let "c" ("print" `Apply` "6") "a")
