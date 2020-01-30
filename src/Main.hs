{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Object)
import qualified Data.ByteString.Char8 as BS
import GitHub.Data.Webhooks.Events
import GitHub.Data.Webhooks.Payload
import Servant
import Servant.GitHub.Webhook
import System.Environment (lookupEnv)
import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)

--
-- Webhook boilerplate
--

main :: IO ()
main = do
    key <- lookupEnv "WEBHOOK_SECRET"
    case key of
        Just k  -> do
            putStrLn "Running server with webhook secret verification"
            run 8080 . verifiedServer . gitHubKey . pure $ BS.pack k
        Nothing -> do
            putStrLn "Running server"
            run 8080 server

type VerifiedAPI = GitHubSignedReqBody '[JSON] Object :> API

verifiedServer :: GitHubKey Object -> Application
verifiedServer key = serveWithContext (Proxy @VerifiedAPI) (key :. EmptyContext) verifiedWebhook

server :: Application
server = serve (Proxy @API) webhook

verifiedWebhook :: ((), Object) -> Webhook
verifiedWebhook _ = webhook

--
-- Webhook implementation
--

type API
    =  "webhook"
    :> GitHubEvent '[ 'WebhookPushEvent ]
    :> ReqBody '[JSON] PushEvent
    :> Post '[JSON] ()

type Webhook = RepoWebhookEvent -> PushEvent -> Handler ()

webhook :: Webhook
webhook e pe = liftIO $ do
    putStrLn $ "Received webhook event: " ++ show e
    print . whRepoCloneUrl $ evPushRepository pe
    print $ evPushHeadSha pe
