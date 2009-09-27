module Network.Protocol.Uri.Remap where

import Control.Category
import Data.List 
import Data.Record.Label
import Network.Protocol.Uri.Data
import Prelude hiding ((.), id, mod)

-- | Map one URI to another using a URI mapping scheme. A URI mapping scheme is
-- simply a pair of URIs of which only the host part, port number and path will
-- be taken into account when mapping.

remap :: (Uri, Uri) -> Uri -> Maybe Uri
remap (f, t) u =
  let
    ftu = [f, t, u]
    hst = _host . authority
    [h0, h1, h2] = map (get hst)      ftu
    [p0, p1, p2] = map (get port)     ftu
    [s0, s1, s2] = map (get segments) ftu
  in case
     ( remapHost h0 h1 h2
     , remapPort p0 p1 p2
     , remapPath s0 s1 s2
     ) of
    (Just h, Just p, Just s)
      -> Just (set hst h . set port p . set segments s $ u)
    _ -> Nothing
  where
  remapHost (Hostname (Domain a))
            (Hostname (Domain b))   (Hostname (Domain c))          = fmap (Hostname . Domain . (++b)) (a `stripPrefix` reverse c)
  remapHost (Hostname (Domain a)) b (Hostname (Domain c)) | a == c = Just b
  remapHost (RegName a)           b (RegName c)           | a == c = Just b
  remapHost (IP a)                b (IP c)                | a == c = Just b
  remapHost _                     _ _                              = Nothing
  remapPath xs ys zs = fmap (ys++) (xs `stripPrefix` zs)
  remapPort x y z = if x == z then Just y else Nothing

-- from = toUri "http://myhost:8080/ggl"
-- to   = toUri "http://google.com/gapp"
-- testRemap =
--   do let x = remap from to (toUri "http://images.myhost:8080/ggl/search?q=aapjes")
--      print x


