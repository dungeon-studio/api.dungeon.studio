module Character.CreationSpec (main, spec) where

import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "character creation" $ do
    context "when correct" $ do
      it "

