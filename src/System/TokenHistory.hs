module System.TokenHistory
    ( Branch(..)
    , GitLine(..)
    , processTokens
    , extractSHA
    ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Text as T
import qualified System.Process as Process
import qualified System.TokenHistory.Parser as Parser
import System.TokenHistory.Types

processTokens :: MonadIO m => T.Text -> m (Either T.Text [GitLine])
processTokens v = Parser.parseInput <$> readGit v

readGit :: MonadIO m => T.Text -> m T.Text
readGit token =
    liftIO $
    T.strip . T.pack <$>
    Process.readProcess
        "git"
        ["log", "-G", T.unpack token, "--pretty=format:%h %as %d %s"]
        []
