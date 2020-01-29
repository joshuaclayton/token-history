module Main where

import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BS
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Options.Applicative
import qualified System.TokenHistory as TokenHistory

newtype Options =
    MkOptions T.Text

optionsToToken :: Options -> T.Text
optionsToToken (MkOptions v) = v

main :: MonadIO m => m ()
main = runProgram =<< parseCLI

runProgram :: MonadIO m => Options -> m ()
runProgram (MkOptions v) =
    liftIO $
    either T.putStrLn (BS.putStr . A.encode) =<< TokenHistory.processTokens v

parseCLI :: MonadIO m => m Options
parseCLI =
    liftIO $ execParser (withInfo parseOptions pHeader pDescription pFooter)
  where
    pHeader = "Token History: Identify Git SHAs for a particular token"
    pDescription =
        "token-history allows a developer to identify Git SHAs that reference \
                  \a particular token"
    pFooter = "CLI USAGE: $ token-history TOKEN"

withInfo :: Parser a -> String -> String -> String -> ParserInfo a
withInfo opts h d f =
    info (helper <*> opts) $ header h <> progDesc d <> footer f

parseOptions :: Parser Options
parseOptions = MkOptions <$> parseToken

parseToken :: Parser T.Text
parseToken = T.pack <$> argument str (metavar "TOKEN")
