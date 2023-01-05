module Modelling.Auxiliary.Common where
import Data.Char                        (isSpace)
import Text.ParserCombinators.Parsec (
  Parser,
  many,
  optional,
  satisfy,
  )

skipSpaces :: Parser ()
skipSpaces = optional $ many $ satisfy isSpace
