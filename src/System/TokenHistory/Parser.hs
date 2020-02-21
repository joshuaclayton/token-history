module System.TokenHistory.Parser
    ( parseInput
    ) where

import Control.Applicative ((<|>))
import Control.Monad (void)
import qualified Data.Bifunctor as BF
import qualified Data.Text as T
import qualified Data.Time as D
import Data.Void (Void)
import System.TokenHistory.Types
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as M
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = M.Parsec Void T.Text

parseInput :: T.Text -> Either T.Text [GitLine]
parseInput =
    BF.first (T.pack . M.errorBundlePretty) .
    M.parse (M.sepBy gitLineParser M.newline <* M.eof) ""

gitLineParser :: Parser GitLine
gitLineParser =
    GitLine <$> (nameParser <* M.newline) <*> (emailParser <* M.newline) <*>
    (shaParser <* M.space) <*>
    (dayParser <* M.space) <*>
    (branchParser <* M.space) <*>
    commitMessageParser

nameParser :: Parser T.Text
nameParser = untilParser '\n'

emailParser :: Parser T.Text
emailParser = untilParser '\n'

untilParser :: Char -> Parser T.Text
untilParser c = M.takeWhileP Nothing (not . (==) c)

shaParser :: Parser SHA
shaParser = MkSHA . T.pack <$> M.some M.hexDigitChar

dayParser :: Parser D.Day
dayParser =
    D.fromGregorian <$> (integerParser <* M.char '-') <*>
    (intParser <* M.char '-') <*>
    intParser

integerParser :: Parser Integer
integerParser = L.lexeme sc L.decimal

intParser :: Parser Int
intParser = fromInteger <$> integerParser

branchParser :: Parser Branch
branchParser = branch <|> noBranch
  where
    branch = Branch . T.pack <$> parens (M.some (M.anySingleBut ')'))
    noBranch = pure NoBranch

commitMessageParser :: Parser T.Text
commitMessageParser = T.pack <$> M.some M.printChar

parens :: Parser a -> Parser a
parens = M.between (symbol "(") (symbol ")")

symbol :: T.Text -> Parser T.Text
symbol = L.symbol sc

sc :: Parser () -- space consumer
sc = L.space (void $ M.char ' ') lineComment blockComment
  where
    lineComment = L.skipLineComment "//"
    blockComment = L.skipBlockComment "/*" "*/"
