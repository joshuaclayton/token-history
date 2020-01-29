module System.TokenHistory.ParserSpec where

import qualified Data.Text as T
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
                [ "aaaa123 Commit message 1"
                , "bbbb123 Commit message 2"
                , "cccc123 (origin/branch-name) Commit message 3"
                ]
        let outcome = parseInput $ T.strip $ T.unlines commits
        outcome `shouldBe`
            Right
                [ GitLine (MkSHA "aaaa123") NoBranch "Commit message 1"
                , GitLine (MkSHA "bbbb123") NoBranch "Commit message 2"
                , GitLine
                      (MkSHA "cccc123")
                      (Branch "origin/branch-name")
                      "Commit message 3"
                ]
