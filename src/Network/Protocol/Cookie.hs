{-# LANGUAGE TemplateHaskell, TypeOperators #-}
-- | For more information: http://www.ietf.org/rfc/rfc2109.txt
module Network.Protocol.Cookie {- todo: test please -}
(

-- * Cookie datatype.
  Cookie (Cookie)
, empty
, cookie
, setCookie

-- * Accessing cookies.
, name
, value
, comment
, commentURL
, discard
, domain
, maxAge
, expires
, path
, port
, secure
, version

-- * Collection of cookies.
, Cookies
, unCookies
, cookies
, setCookies

, pickCookie
, fromList
, toList
)
where

import Prelude hiding ((.), id)
import Control.Category
import Control.Monad (join)
import Data.Record.Label
import Data.Maybe
import Data.Char
import Safe
import Data.List
import Network.Protocol.Uri.Query
import qualified Data.Map as M

-- | The `Cookie` data type containg one key/value pair with all the
-- (potentially optional) meta-data.

data Cookie =
  Cookie
    { _name       :: String
    , _value      :: String
    , _comment    :: Maybe String
    , _commentURL :: Maybe String
    , _discard    :: Bool
    , _domain     :: Maybe String
    , _maxAge     :: Maybe Int
    , _expires    :: Maybe String
    , _path       :: Maybe String
    , _port       :: [Int]
    , _secure     :: Bool
    , _version    :: Int
    } deriving Eq

$(mkLabelsNoTypes [''Cookie])

-- | Access name/key of a cookie.

name :: Cookie :-> String

-- | Access value of a cookie.

value :: Cookie :-> String

-- | Access comment of a cookie.

comment :: Cookie :-> Maybe String

-- | Access comment-URL of a cookie.

commentURL :: Cookie :-> Maybe String

-- | Access discard flag of a cookie.

discard :: Cookie :-> Bool

-- | Access domain of a cookie.

domain :: Cookie :-> Maybe String

-- | Access max-age of a cookie.

maxAge :: Cookie :-> Maybe Int

-- | Access expiration of a cookie.

expires :: Cookie :-> Maybe String

-- | Access path of a cookie.

path :: Cookie :-> Maybe String

-- | Access port of a cookie.

port :: Cookie :-> [Int]

-- | Access secure flag of a cookie.

secure :: Cookie :-> Bool

-- | Access version of a cookie.

version :: Cookie :-> Int

-- | Create an empty cookie.

empty :: Cookie
empty = Cookie "" "" Nothing Nothing False Nothing Nothing Nothing Nothing [] False 0

-- Cookie show instance.

instance Show Cookie where
  showsPrec _ = showsSetCookie

-- Show a semicolon separated list of attribute/value pairs. Only meta pairs
-- with significant values will be pretty printed.

showsSetCookie :: Cookie -> ShowS
showsSetCookie c =
    pair (getL name c) (getL value c)
  . opt  "comment"    (getL comment c)
  . opt  "commentURL" (getL commentURL c)
  . bool "discard"    (getL discard c)
  . opt  "domain"     (getL domain c)
  . opt  "maxAge"     (fmap show (getL maxAge c))
  . opt  "expires"    (getL expires c)
  . opt  "path"       (getL path c)
  . lst  "port"       (map show (getL port c))
  . bool "secure"     (getL secure c)
  . opt  "version"    (optval (getL version c))
  where
    attr a       = showString a
    val v        = showString ("=" ++ v)
    end          = showString "; "
    single a     = attr a . end
    pair a v     = attr a . val v . end
    opt a        = maybe id (pair a)
    lst _ []     = id
    lst a xs     = pair a (intercalate "," xs)
    bool _ False = id
    bool a True  = single a
    optval 0     = Nothing
    optval i     = Just (show i)

showCookie :: Cookie -> String
showCookie c = _name c ++ "=" ++ _value c

parseSetCookie :: String -> Cookie
parseSetCookie s = 
  let p = fw (keyValues ";" "=") s
  in Cookie
    { _name       = (fromMaybe "" .        fmap fst . headMay)              p
    , _value      = (fromMaybe "" . join . fmap snd . headMay)              p
    , _comment    = (                           join . lookup "comment")    p
    , _commentURL = (                           join . lookup "commentURL") p
    , _discard    = (maybe False (const True) . join . lookup "discard")    p
    , _domain     = (                           join . lookup "commentURL") p
    , _maxAge     = (join . fmap readMay .      join . lookup "commentURL") p
    , _expires    = (                           join . lookup "expires")    p
    , _path       = (                           join . lookup "path")       p
    , _port       = (maybe [] (readDef [-1]) .  join . lookup "port")       p
    , _secure     = (maybe False (const True) . join . lookup "secure")     p
    , _version    = (maybe 1 (readDef 1) .      join . lookup "version")    p
    }

parseCookie :: String -> Cookie
parseCookie s =
  let p = fw (values "=") s
  in empty
       { _name  = atDef "" p 0
       , _value = atDef "" p 1
       }

-- | Cookie parser and pretty printer as a lens. To be used in combination with
-- the /Set-Cookie/ header field.

setCookie :: String :<->: Cookie
setCookie = parseSetCookie :<->: show

-- | Cookie parser and pretty printer as a lens. To be used in combination with
-- the /Cookie/ header field.

cookie :: String :<->: Cookie
cookie = parseCookie :<->: showCookie

-- | A collection of multiple cookies. These can all be set in one single HTTP
-- /Set-Cookie/ header field.

data Cookies = Cookies { _unCookies :: M.Map String Cookie }
  deriving Eq

$(mkLabelsNoTypes [''Cookies])

-- | Access raw cookie mapping from collection.

unCookies :: Cookies :-> M.Map String Cookie

instance Show Cookies where
  showsPrec _ = showsSetCookies

showsSetCookies :: Cookies -> ShowS
showsSetCookies =
      is (showString ", ")
    . map (shows . snd)
    . M.toList
    . getL unCookies
  where
  is _ []     = id
  is s (x:xs) = foldl (\a b -> a.s.b) x xs

-- | Cookies parser and pretty printer as a lens.

setCookies :: String :<->: Cookies
setCookies = (fromList :<->: toList) . (map parseSetCookie :<->: map show) . values ","

-- | Label for printing and parsing collections of cookies.

cookies :: String :<->: Cookies
cookies = (fromList :<->: toList) . (map parseCookie :<->: map showCookie) . values ";"

-- | Case-insensitive way of getting a cookie out of a collection by name.

pickCookie :: String -> Cookies :-> Maybe Cookie
pickCookie n = lookupL (map toLower n) . unCookies
  where lookupL k = lens (M.lookup k) (flip M.alter k . const)

-- | Convert a list to a cookies collection.

fromList :: [Cookie] -> Cookies
fromList = Cookies . M.fromList . map (\a -> (map toLower (getL name a), a))

-- | Get the cookies as a list.

toList :: Cookies -> [Cookie]
toList = map snd . M.toList . getL unCookies

