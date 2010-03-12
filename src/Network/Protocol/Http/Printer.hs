{-# LANGUAGE FlexibleInstances #-}
module Network.Protocol.Http.Printer
( showRequestLine
, showResponseLine
)
where {- doc ok -}

import Network.Protocol.Http.Data
import Network.Protocol.Http.Status

instance Show (Http Request) where
  showsPrec _ r@(Http Request {} _ hs) =
    showRequestLine r . shows hs . eol

instance Show (Http Response) where
  showsPrec _ r@(Http Response {} _ hs) =
    showResponseLine r . shows hs . eol

-- | Show HTTP request status line.

showRequestLine :: Http Request -> String -> String
showRequestLine (Http (Request m u) v _) =
    shows m . ss " " . ss u . ss " "
  . shows v . eol

-- | Show HTTP response status line.

showResponseLine :: Http Response -> String -> String
showResponseLine (Http (Response s) v _) =
    shows v . ss " "
  . shows (codeFromStatus s)
  . ss " " . shows s . eol

instance Show Headers where
  showsPrec _ =
      foldr (\a b -> a . eol . b) id
    . map (\(k, a) -> ss k . ss ": " . ss a)
    . unHeaders

instance Show Version where
  showsPrec _ (Version a b) = ss "HTTP/" . shows a . ss "." . shows b

eol :: ShowS
eol = ss "\r\n"

ss :: String -> ShowS
ss = showString

