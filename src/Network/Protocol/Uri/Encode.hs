{-# LANGUAGE TypeOperators #-}
module Network.Protocol.Uri.Encode where

import Data.Bits
import Data.Char
import Data.Maybe
import Data.Label
import Network.Protocol.Uri.Chars
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.UTF8 as U

-- | Decoding and encoding as a label.

encoded :: Bijection (->) String String
encoded = Bij decode encode

-- | URI encode an UTF8 String.

encode :: String -> String
encode = encodeString . C.unpack . U.fromString
  where

-- | URI decode to a UTF8 String.

decode :: String -> String
decode = U.toString . B.pack . map (fromIntegral . ord) . decodeString
  where

-- | Encode a plain String.

encodeString :: String -> String
encodeString = concatMap encodeChar
  where
  encodeChar :: Char -> String
  encodeChar c
    | unreserved c || genDelims c || subDelims c = [c]
    | otherwise = '%' :
        intToDigit (shiftR (ord c) 4) :
        intToDigit ((ord c) .&. 0x0F) : []

-- | Decode a plain String.

decodeString :: String -> String
decodeString [] = []
decodeString ('%':d:e:ds) | isHexDigit d && isHexDigit e = (chr $ digs d * 16 + digs e) : decodeString ds 
  where digs a = fromJust $ lookup (toLower a) $ zip "0123456789abcdef" [0..]
decodeString (d:ds) = d : decodeString ds

