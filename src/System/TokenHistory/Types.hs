module System.TokenHistory.Types
    ( SHA(..)
    , Branch(..)
    , GitLine(..)
    , extractSHA
    ) where

import Data.Aeson ((.=))
import qualified Data.Aeson as A
import qualified Data.Text as T
import qualified Data.Time as D

newtype SHA =
    MkSHA T.Text
    deriving (Show, Eq)

data Branch
    = NoBranch
    | Branch T.Text
    deriving (Show, Eq)

data GitLine = GitLine
    { gAuthorName :: T.Text
    , gAuthorEmail :: T.Text
    , gSHA :: SHA
    , gDate :: D.Day
    , gBranch :: Branch
    , gCommitMessage :: T.Text
    } deriving (Show, Eq)

instance A.ToJSON GitLine where
    toJSON gl =
        A.object
            [ "sha" .= A.String (extractSHA (gSHA gl))
            , "authorName" .= A.String (gAuthorName gl)
            , "authorEmail" .= A.String (gAuthorEmail gl)
            , "date" .= A.toJSON (gDate gl)
            , "branch" .= A.toJSON (gBranch gl)
            , "commitMessage" .= A.String (gCommitMessage gl)
            ]

instance A.ToJSON Branch where
    toJSON NoBranch = A.Null
    toJSON (Branch t) = A.String t

extractSHA :: SHA -> T.Text
extractSHA (MkSHA v) = v
