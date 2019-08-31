module Main (main) where

import Test.Hspec
import LispVal

import Parser

main :: IO ()
main = hspec $ do
  describe "readExpr" $ do
    it "reads atom" $ do
      readExpr "b" `shouldBe` (Right $ Atom "b")

    it "reads Number" $ do
      readExpr "1" `shouldBe` (Right $ Number 1)

    it "reads String" $ do
      readExpr "\"what\"" `shouldBe` (Right $ String "what")

    it "reads Bool true" $ do
      readExpr "#t" `shouldBe` (Right $ Bool True)

    it "reads Bool true" $ do
      readExpr "#f" `shouldBe` (Right $ Bool False)

    it "reads Conditional" $ do
      readExpr "(if (< 3 5) 2 #t)"
        `shouldBe`
        (Right $ Cond
         (List [Atom "<", Number 3, Number 5])
         (Number 2)
         (Bool True))

    it "reads List" $ do
      readExpr "(1 2 (+ 3 4) 4)"
        `shouldBe`
        (Right $ List [Number 1, Number 2,
                       List [Atom "+", Number 3, Number 4],
                       Number 4])

    it "fails with non well formed gabelisp: unbalanced right parens" $ do
      (show $ readExpr "(2 (+ 3 4) 4")
        `shouldBe`
        "Left Unbalanced Parens"

    it "fails with non well formed gabelisp" $ do
      (show $ readExpr "a ((+ 3 4) 4)")
        `shouldBe`
        "Left Parse error at \"lisp\" (line 1, column 2):\nunexpected ' '\nexpecting letter, digit or end of input"

    it "fails with non well formed gabelisp: unbalanced parens" $ do
      (show $ readExpr "(a (+ 3 4) 4)))")
        `shouldBe`
        "Left Unbalanced Parens"
