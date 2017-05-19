-- * An example
--
-- | Here is an example of a full program which writes a message to
-- syslog once a second proclaiming its continued existance, and
-- which installs its own SIGHUP handler.  Note that you won't
-- actually see the message once a second in the log on most
-- systems.  @syslogd@ detects repeated messages and prints the
-- first one, then delays for the rest and eventually writes a line
-- about how many times it has seen it.

{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.Posix.Daemonize (CreateDaemon(..), serviced, simpleDaemon, syslog)
import System.Posix.Signals (installHandler, Handler(Catch), sigHUP, fullSignalSet)
import System.Posix.Syslog (Priority(Notice))
import Control.Concurrent (threadDelay)
import Control.Monad (forever)

main :: IO ()
main = serviced stillAlive

stillAlive :: CreateDaemon ()
stillAlive = simpleDaemon { program = stillAliveMain }

stillAliveMain :: () -> IO ()
stillAliveMain _ = do
  installHandler sigHUP (Catch taunt) (Just fullSignalSet)
  forever $ do threadDelay (10^6)
               syslog Notice "I'm still alive!"

taunt :: IO ()
taunt = syslog Notice "I sneeze in your general direction, you and your SIGHUP."
