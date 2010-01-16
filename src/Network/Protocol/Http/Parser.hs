{-# LANGUAGE TypeOperators, FlexibleContexts #-}
module Network.Protocol.Http.Parser {- doc ok -}
  (

  -- * Top level message parsers.

    parseRequest
  , parseResponse
  , parseHeaders

  -- * Exposure of internal parsec parsers.

  , pRequest
  , pResponse
  , pHeaders
  , pVersion
  , pMethod

  ) where

import Control.Applicative hiding (empty)
import Data.Char
import Data.List hiding (insert)
import Network.Protocol.Http.Data
import Network.Protocol.Http.Status
import Text.Parsec hiding (many, (<|>))

-- | Parse a string as an HTTP request message. This parser is very forgiving.

parseRequest :: String -> Either String (Http Request)
parseRequest = either (Left . show) (Right . id) . parse pRequest ""

-- | Parse a string as an HTTP request message. This parser is very forgiving.

parseResponse :: String -> Either String (Http Response)
parseResponse = either (Left . show) (Right . id) . parse pResponse ""

-- | Parse a string as a list of HTTP headers.

parseHeaders :: String -> Either String Headers
parseHeaders = either (Left . show) (Right . id) . parse pHeaders ""

-- | Parsec parser to parse the header part of an HTTP request.

pRequest :: Stream s m Char => ParsecT s u m (Http Request)
pRequest =
      (\m u v h -> Http (Request m u) v h)
  <$> (pMethod <* many1 (oneOf ls))
  <*> (many1 (noneOf ws) <* many1 (oneOf ls))
  <*> (pVersion <* eol)
  <*> (pHeaders <* eol)

-- | Parsec parser to parse the header part of an HTTP response.

pResponse :: Stream s m Char => ParsecT s u m (Http Response)
pResponse =
      (\v s h -> Http (Response (statusFromCode $ read s)) v h)
  <$> (pVersion <* many1 (oneOf ls))
  <*> (many1 digit <* many1 (oneOf ls) <* many1 (noneOf lf) <* eol)
  <*> (pHeaders <* eol)

-- | Parsec parser to parse one or more, possibly multiline, HTTP header lines.

pHeaders :: Stream s m Char => ParsecT s u m Headers
pHeaders = Headers <$> p
  where
    p = (\k v -> ((k, v):))
        <$> many1 (noneOf (':':ws)) <* string ":"
        <*> (intercalate ws <$> (many $ many1 (oneOf ls) *> many1 (noneOf lf) <* eol))
        <*> option [] p

-- | Parsec parser to parse HTTP versions. Recognizes X.X versions only.

pVersion :: Stream s m Char => ParsecT s u m Version
pVersion = 
      (\h l -> Version (ord h - ord '0') (ord l  - ord '0'))
  <$> (istring "HTTP/" *> digit)
  <*> (char '.'       *> digit)

-- | Parsec parser to parse an HTTP method. Parses arbitrary method but
-- actually recognizes the ones listed as a constructor for `Method'.

pMethod :: Stream s m Char => ParsecT s u m Method
pMethod =
     choice
   $ map (\a -> a <$ (try . istring . show $ a)) methods
  ++ [OTHER <$> many (noneOf ws)]

-- Helpers.

lf, ws, ls :: String
lf = "\r\n"
ws = " \t\r\n"
ls = " \t"

-- Optional parser with maybe result.

pMaybe :: Stream s m t => ParsecT s u m a -> ParsecT s u m (Maybe a)
pMaybe a = option Nothing (Just <$> a)

-- Parse end of line, \r, \n or \r\n.

eol :: Stream s m Char => ParsecT s u m ()
eol = () <$ ((char '\r' <* pMaybe (char '\n')) <|> char '\n')

-- Case insensitive string parser.

istring :: Stream s m Char => String -> ParsecT s u m String
istring s = sequence (map (\c -> satisfy (\d -> toUpper c == toUpper d)) s)

