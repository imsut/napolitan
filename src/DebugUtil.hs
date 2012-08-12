{-# LANGUAGE ImplicitPrelude #-}
module DebugUtil where

import Yesod.Logger (Logger, logString, defaultDevelopmentLogger)

debugLog :: String -> IO ()
debugLog msg = defaultDevelopmentLogger >>= flip logString msg
