{-# LANGUAGE TemplateHaskell, TypeOperators #-}
module Network.Protocol.Http.Data where

import Control.Category
import Data.Char
import Data.List
import Data.List.Split
import Data.Label
import Network.Protocol.Http.Status
import Network.Protocol.Uri
import Prelude hiding ((.), id, lookup)

-- | List of HTTP request methods.

data Method =
    OPTIONS
  | GET
  | HEAD
  | POST
  | PUT
  | DELETE
  | TRACE
  | CONNECT
  | OTHER String
  deriving (Show, Eq)

-- | HTTP protocol version.

data Version = Version {_major :: Int, _minor :: Int}
  deriving (Eq, Ord)

type Key   = String
type Value = String

-- | HTTP headers as mapping from keys to values.

newtype Headers = Headers { unHeaders :: [(Key, Value)] } -- order seems to matter
  deriving Eq

-- | Request specific part of HTTP messages.

data Request = Request  { __method :: Method, __uri :: String }
  deriving Eq

-- | Response specific part of HTTP messages.

data Response = Response { __status :: Status }
  deriving Eq

-- | An HTTP message. The message body is *not* included.

data Http a = Http
  { _headline :: a
  , _version  :: Version
  , _headers  :: Headers
  } deriving Eq

-- | All recognized method constructors as a list.

methods :: [Method]
methods = [OPTIONS, GET, HEAD, POST, PUT, DELETE, TRACE, CONNECT]

-- | Create HTTP 1.0 version.

http10 :: Version
http10 = Version 1 0

-- | Create HTTP 1.1 version.

http11 :: Version
http11 = Version 1 1

-- | Create an empty set of headers.

emptyHeaders :: Headers
emptyHeaders = Headers []

-- | Create an empty HTTP request message.

emptyRequest :: Http Request
emptyRequest = Http (Request GET "") http11 emptyHeaders

-- | Create an empty HTTP response message.

emptyResponse :: Http Response
emptyResponse = Http (Response OK) http11 emptyHeaders

$(mkLabelsNoTypes [''Version, ''Request, ''Response, ''Http])

-- | Label to access the major part of the version.

major :: Version :-> Int

-- | Label to access the minor part of the version.

minor :: Version :-> Int

-- Internal helper labels.

_uri    :: Request  :-> String
_method :: Request  :-> Method
_status :: Response :-> Status

-- | Label to access the header of an HTTP message.

headers :: Http a :-> Headers

-- | Label to access the version part of an HTTP message.

version :: Http a :-> Version

-- | Label to access the header line part of an HTTP message.

headline :: Http a :-> a

-- | Label to access the method part of an HTTP request message.

method :: Http Request :-> Method
method = _method . headline

-- | Label to access the URI part of an HTTP request message.

uri :: Http Request :-> String
uri = _uri . headline

-- | Label to access the URI part of an HTTP request message and access it as a
-- true URI data type.

asUri :: Http Request :-> Uri
asUri = Bij toUri show `iso` uri

-- | Label to access the status part of an HTTP response message.

status :: Http Response :-> Status
status = _status . headline

-- | Normalize the capitalization of an HTTP header key.

normalizeHeader :: Key -> Key
normalizeHeader = intercalate "-" . map casing . splitOn "-"
  where
  casing ""     = ""
  casing (x:xs) = toUpper x : map toLower xs

-- | Generic lens to access an HTTP header field by key.

header :: Key -> Http a :-> Maybe Value
header key = lens
  (lookup (normalizeHeader key) . unHeaders . get headers)
  (\x -> modify headers (Headers . alter (normalizeHeader key) x . unHeaders))
  where
  alter :: Eq a => a -> Maybe b -> [(a, b)] -> [(a, b)]
  alter k v []                      = maybe [] (\w -> (k, w):[]) v
  alter k v ((x, y):xs) | k == x    = maybe xs (\w -> (k, w):xs) v
                        | otherwise = (x, y) : alter k v xs

