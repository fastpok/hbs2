import Lucid
import Network.Wai.Middleware.Static
import Pages.Main
import Web.Scotty

main :: IO ()
main = scotty 3000 $ do
  middleware $ staticPolicy (addBase "static")
  get "/" $ html $ renderText mainPage
