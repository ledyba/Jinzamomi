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
