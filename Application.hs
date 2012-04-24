{-# OPTIONS_GHC -fno-warn-orphans #-}
module Application
    ( makeApplication
    , getApplicationDev
    ) where

import Import
import Settings
import Yesod.Auth
import Yesod.Default.Config
import Yesod.Default.Main
import Yesod.Default.Handlers
#if DEVELOPMENT
import Yesod.Logger (Logger, logBS)
import Network.Wai.Middleware.RequestLogger (logCallbackDev)
#else
import Yesod.Logger (Logger, logBS, toProduction)
import Network.Wai.Middleware.RequestLogger (logCallback)
#endif
import Network.HTTP.Conduit (newManager, def)

-- Import all relevant handler modules here.
-- Don't forget to add new modules to your cabal file!
import Handler.Home

-- This line actually creates our YesodSite instance. It is the second half
-- of the call to mkYesodData which occurs in Foundation.hs. Please see
-- the comments there for more details.
mkYesodDispatch "App" resourcesApp

-- This function allocates resources (such as a database connection pool),
-- performs initialization and creates a WAI application. This is also the
-- place to put your migrate statements to have automatic database
-- migrations handled by Yesod.
makeApplication :: AcidState AppState -> AppConfig DefaultEnv Extra -> Logger -> IO Application
makeApplication acid conf logger = do
    foundation <- makeFoundation acid conf setLogger
    app <- toWaiApp foundation
    return $ logWare app
  where
#ifdef DEVELOPMENT
    logWare = logCallbackDev (logBS setLogger)
    setLogger = logger
#else
    setLogger = toProduction logger -- by default the logger is set for development
    logWare = logCallback (logBS setLogger)
#endif


makeFoundation :: AcidState AppState -> AppConfig DefaultEnv Extra -> Logger -> IO App
makeFoundation acid conf setLogger = do
    manager <- newManager def
    s <- staticSite
    return $ App conf setLogger s manager acid


-- for yesod devel
getApplicationDev :: IO (Int, Application)
getApplicationDev = do
    acid <- openLocalState emptyState
    putStrLn "============================================="
    createCheckpoint acid
    putStrLn "============================================="
    defaultDevelApp loader (makeApplication acid)
  where
    loader = loadConfig (configSettings Development)
        { csParseExtra = parseExtra
        }
