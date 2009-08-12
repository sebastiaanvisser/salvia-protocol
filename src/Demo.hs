module Main where

import Control.Monad.Trans
import Network.Socket
import Network.Salvia.Httpd
import Network.Salvia.Handlers

main :: IO ()
main =
  do addr <- inet_addr "127.0.0.1"
     putStrLn "started"
     server 
       (defaultConfig { listenAddr = addr, listenPort = 8080 })
       (hDefaultEnv myHandler)
       ()

-- Serve the current directory.

myHandler :: (MonadIO m, RequestM m, ResponseM m, SendM m) => m ()
myHandler = hFileSystem "."



test :: IO (Maybe String)
test = getRequest "http://www.google.nl/search?q=aap"


