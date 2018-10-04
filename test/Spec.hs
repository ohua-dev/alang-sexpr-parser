{-# LANGUAGE OverloadedLists    #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
import Ohua.Prelude

import           Data.ByteString.Lazy     as B
import           Ohua.ALang.Lang
import           Ohua.ALang.NS
import           Ohua.Compat.SExpr.Lexer
import           Ohua.Compat.SExpr.Parser
import           Test.Hspec

deriving instance Show a => Show (Namespace a)
deriving instance Eq a => Eq (Namespace a)

lp :: B.ByteString -> Expr SomeBinding
lp = parseExp . tokenize

main :: IO ()
main = hspec $
    describe "parser and lexer" $ do
        it "parse an apply" $
            lp "(something a b c)" `shouldBe` ("something" `Apply` "a" `Apply` "b" `Apply` "c")
        it "parses a let" $
            lp "(let [a b] b)" `shouldBe` Let "a" "b" "b"
        it "parses a lambda" $
            lp "(fn [a [b c]] (print a) c)" `shouldBe` Lambda "a" (Lambda (Destructure ["b", "c"]) $ Let "_" ("print" `Apply` "a") "c")
        it "parses an identifier with strange symbols" $
            lp "(let [a-b a0] -)" `shouldBe` Let "a-b" "a0" "-"
        it "parses longer let binds" $
            lp "(let [a 0 b 1 c (print 6)] a)" `shouldBe` Let "a" "0" (Let "b" "1" $ Let "c" ("print" `Apply` "6") "a")

        it "parses the example module" $ (parseNS . tokenize <$> B.readFile "test-resources/something.ohuas")
            `shouldReturn`
            Namespace ["some_ns"]
                [ (["some","module"], ["a"]) ]
                [ (["ohua","math"],["add","isZero"]) ]
                [ ("square", Lambda "x" ("add" `Apply` "x" `Apply` "x"))
                , ("algo1", Lambda "someParam" $
                        Let "a" ("square" `Apply` "someParam") $
                        Let "coll0" ("ohua.lang/smap" `Apply` Lambda "i" ("square" `Apply` "i") `Apply` "coll")
                        ("ohua.lang/if"
                            `Apply` ("isZero" `Apply` "a")
                            `Apply` Lambda "_" "coll0"
                            `Apply` Lambda "_" "a"))
                , ("main", Lambda "param"  ("algo0" `Apply` "param"))
                ]
