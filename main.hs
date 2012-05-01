import Prelude              (IO, (.))
import Yesod.Default.Config (fromArgs)
import Yesod.Default.Main   (defaultMain)
import Settings             (parseExtra)
import Application          (makeApplication)
import Data.Acid            (openLocalState, createCheckpoint)
import State                (emptyState)
import Control.Exception    (bracket)

main :: IO ()
main = bracket (openLocalState emptyState)
               (defaultMain (fromArgs parseExtra) . makeApplication)
               createCheckpoint
