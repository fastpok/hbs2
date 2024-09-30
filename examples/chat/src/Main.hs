import Config
import Control.Monad.Reader
import Env
import Message
import Monad
import Network.Wai.Middleware.Static
import Pages.Login
import Pages.Main
import Web.Scotty.Trans

main :: IO ()
main = scottyT 3000 runIO application
  where
    runIO :: AppM a -> IO a
    runIO m = do
      config <- getConfig
      withEnv config $ runReaderT $ runAppM m

application :: ScottyT AppM ()
application = do
  middleware $ staticPolicy (noDots >-> addBase "static")
  get "/" mainPage
  get "/login" loginPage
  post "/message" $ postMessage