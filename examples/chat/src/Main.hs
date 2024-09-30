import Config
import Control.Monad.Reader
import Env
import Error
import Message
import Monad
import Network.Wai.Middleware.Static
import Pages.Login
import Pages.Main
import Web.Scotty.Trans

main :: IO ()
main = do
  config <- getConfig
  env <- initEnv config
  scottyT 3000 (runIO env) application
  where
    runIO :: Env -> AppM a -> IO a
    runIO env m = runReaderT (runAppM m) env

application :: ScottyT AppM ()
application = do
  middleware $ staticPolicy (noDots >-> addBase "static")
  defaultHandler exceptionHandler
  get "/" mainPage
  get "/login" loginPage
  post "/:chat/message" $ postMessage
  get "/:chat/messages" $ getMessages