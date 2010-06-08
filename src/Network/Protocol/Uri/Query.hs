{-# LANGUAGE TypeOperators #-}
module Network.Protocol.Uri.Query where

import Prelude hiding ((.), id)
import Control.Category
import Data.List
import Data.List.Split 
import Data.Record.Label
import Network.Protocol.Uri.Data
import Network.Protocol.Uri.Encode

type Parameters = [(String, Maybe String)]

-- | Fetch the query parameters form a URI.

queryParams :: Uri :-> Parameters
queryParams = params % _query

-- | Generic lens to parse/print a string as query parameters.

params :: String :<->: Parameters
params = keyValues "&" "=" . (from :<->: to) . encoded
  where from = intercalate " " . splitOn "+"
        to   = intercalate "+" . splitOn " "

-- | Generic label for accessing key value pairs encoded in a string.

keyValues :: String -> String -> String :<->: Parameters
keyValues sep eqs = parser :<->: printer
  where parser =
            filter (\(a, b) -> not (null a) || b /= Nothing && b /= Just "")
          . map (f . splitOn eqs)
          . concat
          . map (splitOn sep)
          . lines
          where f []     = ("", Nothing)
                f [x]    = (trim x, Nothing)
                f (x:xs) = (trim x, Just . trim $ intercalate eqs xs)
        printer = intercalate sep . map (\(a, b) -> a ++ maybe "" (eqs ++) b)

-- | Generic label for accessing lists of values encoded in a string.

values :: String -> String :<->: [String]
values sep = parser :<->: printer
  where parser = filter (not . null) . concat . map (map trim . splitOn sep) . lines
        printer = intercalate sep

-- Helper to trim all heading and trailing whitespace.

trim :: String -> String
trim = rev (dropWhile (`elem` " \t\n\r"))
  where rev f = reverse . f . reverse . f

