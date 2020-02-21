module System.TokenHistory.ParserSpec where

import qualified Data.Text as T
import qualified Data.Time as D
import System.TokenHistory.Parser
import System.TokenHistory.Types
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec =
    describe "System.TokenSearch.Parser" $
    it "parses input correctly" $ do
        let commits =
                [ "Person\nperson@example.com\naaaa123 2020-01-10 Commit message 1"
                , "Person\nperson@example.com\nbbbb123 2019-12-20 Commit message 2"
                , "Other\nother@example.com\ncccc123 2019-11-01 (origin/branch-name) Commit message 3"
                ]
        let outcome = parseInput $ T.strip $ T.unlines commits
        outcome `shouldBe`
            Right
                [ GitLine
                      "Person"
                      "person@example.com"
                      (MkSHA "aaaa123")
                      (d 2020 1 10)
                      NoBranch
                      "Commit message 1"
                , GitLine
                      "Person"
                      "person@example.com"
                      (MkSHA "bbbb123")
                      (d 2019 12 20)
                      NoBranch
                      "Commit message 2"
                , GitLine
                      "Other"
                      "other@example.com"
                      (MkSHA "cccc123")
                      (d 2019 11 1)
                      (Branch "origin/branch-name")
                      "Commit message 3"
                ]

d :: Integer -> Int -> Int -> D.Day
d = D.fromGregorian
