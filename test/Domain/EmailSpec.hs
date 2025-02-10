module Domain.EmailSpec (spec) where

import Test.Hspec

import Domain.Email (makeEmail, EmailError (..), unsafeMakeEmail)

spec :: Spec
spec = do
  describe "文字列からEmailを生成する" do
    it "文字列が妥当なメールアドレスの場合、Emailが返る" do
      makeEmail "test@test.com" `shouldBe` Right (unsafeMakeEmail "test@test.com")

    it "文字列が不正なメールアドレスの場合、エラーが返る" do
      makeEmail "test.com" `shouldSatisfy` \case
        Left (InvalidEmailFormat _) -> True
        _ -> False