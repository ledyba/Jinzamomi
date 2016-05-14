{-# LANGUAGE OverloadedStrings #-}
module IRSpec (spec) where

import Test.Hspec
import Uzume.Driver.IR

spec :: Spec
spec =
  describe "basic test" $ do
    it "value test" $ do
      compile (Var "test") `shouldBe` "test;"
      compile (Dot (Var "test") "value") `shouldBe` "test.value;"
      compile (Block [Var "test"]) `shouldBe` "{\n  test;\n}"
      compile (Try (Block [Var "test"]) "e" (Block [Var "test2"])) `shouldBe` "try\n{\n  test;\n}\ncatch(e)\n{\n  test2;\n}"
