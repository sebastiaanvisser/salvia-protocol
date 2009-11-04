module Network.Protocol.Uri.Encode where

import Data.Bits
import Data.Char
import Data.Maybe
import Data.Record.Label
import Network.Protocol.Uri.Chars
import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as U

-- | URI encode a string.

encode :: String -> String
encode = concatMap encodeChr
  where
    encodeChr c
      | unreserved c || genDelims c || subDelims c = [c]
      | otherwise = '%' :
          intToDigit (shiftR (ord c) 4) :
          intToDigit ((ord c) .&. 0x0F) : []

-- | URI decode a string.

decode :: String -> String
decode = U.toString . B.pack . map (fromIntegral . ord) . dec
  where
    dec [] = []
    dec ('%':d:e:ds) | isHexDigit d && isHexDigit e = (chr $ digs d * 16 + digs e) : dec ds 
      where digs a = fromJust $ lookup (toLower a) $ zip "0123456789abcdef" [0..]
    dec (d:ds) = d : dec ds

-- | Decoding and encoding as a label.

encoded :: String :<->: String
encoded = decode <-> encode

