{-# LANGUAGE TypeOperators #-}
module Network.Protocol.Http.Headers where {- doc ok -}

import Control.Category
import Control.Monad
import Data.Label
import Network.Protocol.Http.Data
import Network.Protocol.Uri.Query
import Prelude hiding ((.), id)
import Safe

-- | Access the /Content-Length/ header field.

contentLength :: (Read i, Integral i, Show i) => Http a :-> Maybe i
contentLength = Bij (join . fmap readMay)  (fmap show) `iso` header "Content-Length"

-- | Access the /Connection/ header field.

connection :: Http a :-> Maybe String
connection = header "Connection"

lmap l = let (Bij a b) = l in Bij (fmap a) (fmap b)

-- | Access the /Accept/ header field.

accept :: Http a :-> Maybe Parameters
accept = lmap (keyValues "," ";") `iso` header "Accept"

-- | Access the /Accept-Encoding/ header field.

acceptEncoding :: Http a :-> Maybe [String]
acceptEncoding = lmap (values ",") `iso` header "Accept-Encoding"

-- | Access the /Accept-Language/ header field.

acceptLanguage :: Http a :-> Maybe [String]
acceptLanguage = lmap (values ",") `iso` header "Accept-Language"

-- | Access the /Connection/ header field.

cacheControl :: Http a :-> Maybe String
cacheControl = header "Cache-Control"

-- | Access the /Keep-Alive/ header field.

keepAlive :: (Read i, Integral i, Show i) => Http a :-> Maybe i
keepAlive = Bij (join . fmap readMay) (fmap show) `iso` header "Keep-Alive"

-- | Access the /Cookie/ header field.

cookie :: Http Request :-> Maybe String
cookie = header "Cookie"

-- | Access the /Set-Cookie/ header field.

setCookie :: Http Response :-> Maybe String
setCookie = header "Set-Cookie"

-- | Access the /Location/ header field.

location :: Http a :-> Maybe String
location = header "Location"

-- | Access the /Content-Type/ header field. The content-type will be parsed
-- into a mimetype and optional charset.

contentType :: Http a :-> Maybe (String, Maybe String)
contentType = 
    Bij parser (fmap printer)
  `iso` lmap (keyValues ";" "=")
  `iso` header "Content-Type"
  where 
    printer (x, y) = (x, Nothing) : maybe [] (\z -> [("charset", Just z)]) y
    parser (Just ((m, Nothing):("charset", c):_)) = Just (m, c)
    parser _                                      = Nothing

-- | Access the /Date/ header field.

date :: Http a :-> Maybe String
date = header "Date"

-- | Access the /Host/ header field.

hostname :: Http a :-> Maybe String
hostname = header "Host"

-- | Access the /Server/ header field.

server :: Http a :-> Maybe String
server = header "Server"

-- | Access the /User-Agent/ header field.

userAgent :: Http a :-> Maybe String
userAgent = header "User-Agent"

-- | Access the /Upgrade/ header field.

upgrade :: Http a :-> Maybe String
upgrade = header "Upgrade"

-- | Access the /Last-Modified/ header field.

lastModified :: Http a :-> Maybe Value
lastModified = header "Last-Modified"

-- | Access the /Accept-Ranges/ header field.

acceptRanges :: Http a :-> Maybe Value
acceptRanges = header "Accept-Ranges"

-- | Access the /Referer/ header field.

referer :: Http a :-> Maybe Value
referer = header "Referer"

-- | Access the /Origin/ header field.

origin :: Http a :-> Maybe Value
origin = header "Origin"

-- | Access the /ETag/ header field.

eTag :: Http a :-> Maybe Value
eTag = header "ETag"

