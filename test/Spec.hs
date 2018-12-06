{-# LANGUAGE OverloadedLists    #-}
{-# LANGUAGE OverloadedStrings  #-}
import Ohua.Prelude

import Data.ByteString.Lazy as B
import Ohua.Compat.SExpr.Lexer
import Ohua.Compat.SExpr.Parser
import Ohua.Frontend.Lang
import Ohua.Frontend.NS
import Test.Hspec


lp :: B.ByteString -> Expr
lp = parseExp . tokenize

main :: IO ()
main =
    hspec $
    describe "parser and lexer" $ do
        it "parse an apply" $
            lp "(something a b c)" `shouldBe` (AppE "something" ["a", "b", "c"])
        it "parses a let" $ lp "(let [a b] b)" `shouldBe` LetE "a" "b" "b"
        describe "literals" $ do
            it "parses nil as unit" $
                lp "nil" `shouldBe` LitE UnitLit
            it "parses integers" $ do
                lp "0" `shouldBe` LitE (NumericLit 0)
                lp "1" `shouldBe` LitE (NumericLit 1)
                lp "4" `shouldBe` LitE (NumericLit 4)
                lp "100040" `shouldBe` LitE (NumericLit 100040)
            it "parses negative integers" $ do
                lp "-1" `shouldBe` LitE (NumericLit (-1))
                lp "-4" `shouldBe` LitE (NumericLit (-4))
                lp "-100040" `shouldBe` LitE (NumericLit (-100040))
            it "unit is a valid pattern" $
                lp "(fn [nil] a)" `shouldBe` LamE [UnitP] "a"
        it "parses a lambda" $
            lp "(fn [a [b c]] (print a) c)" `shouldBe`
            LamE ["a", ["b", "c"]] (StmtE (AppE "print" ["a"]) "c")
        it "parses an identifier with strange symbols" $
            lp "(let [a-b a0] -)" `shouldBe` LetE "a-b" "a0" "-"
        it "parses longer let binds" $
            let n = LitE . NumericLit in
            lp "(let [a 0 b 1 c (print 6)] a)" `shouldBe`
            LetE "a" (n 0) (LetE "b" (n 1) $ LetE "c" (AppE "print" [n 6]) "a")
        it "parses the example module" $
            (parseNS . tokenize <$> B.readFile "test-resources/something.ohuas") `shouldReturn`
            ((emptyNamespace ["some_ns"] :: Namespace ()) &
             algoImports .~ [(["some", "module"], ["a"])] &
             sfImports .~ [(["ohua", "math"], ["add", "isZero"])] &
             decls .~
             [ ("square", LamE ["x"] (AppE "add" ["x", "x"]))
             , ( "algo1"
               , LamE ["someParam"] $
                 LetE "a" (AppE "square" ["someParam"]) $
                 LetE
                     "coll0"
                     (AppE
                          (LitE $ FunRefLit $ FunRef "ohua.lang/smap" Nothing)
                          [LamE ["i"] (AppE "square" ["i"]), "coll"])
                     (IfE (AppE "isZero" ["a"]) "coll0" "a"))
             , ("main", LamE ["param"] (AppE "algo0" ["param"]))
             ])
