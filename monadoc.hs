-- TODO
-- [ ] Handle request/response headers.
-- [ ] Handle query strings.
-- [ ] Rig everything up in Terraform.
-- [ ] Build and deploy from Semaphore.

module Main ( main ) where

import qualified Control.Exception
import qualified Control.Monad
import qualified Control.Monad.Fail
import qualified Data.Aeson
import qualified Data.Aeson.Text
import qualified Data.Aeson.Types
import qualified Database.MongoDB
import qualified Database.PostgreSQL.Simple
import qualified Data.ByteString
import qualified Data.ByteString.Base64
import qualified Data.ByteString.Lazy
import qualified Data.CaseInsensitive
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Data.Text.Encoding.Error
import qualified Network.HTTP.Client
import qualified Network.HTTP.Types
import qualified System.Environment

-- <https://docs.aws.amazon.com/lambda/latest/dg/runtimes-custom.html>
-- <https://docs.aws.amazon.com/lambda/latest/dg/runtimes-api.html>
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/set-up-lambda-proxy-integrations.html>
main :: IO ()
main = do
  requireMongoDB
  requirePostgreSQL

  manager <- getManager
  apiUrl <- getApiUrl

  Control.Exception.handle (sendInitializationError manager apiUrl) $ do
    lambda <- getLambda

    Control.Monad.forever $ do
      invocation <- makeApiRequest manager apiUrl "/runtime/invocation/next" Nothing
      requestId <- getRequestId invocation
      request <- either fail pure . Data.Aeson.eitherDecode $ Network.HTTP.Client.responseBody invocation

      Control.Exception.handle (sendInvocationError manager apiUrl requestId) $ do
        response <- lambda request
        sendInvocationResponse manager apiUrl requestId response

getRequestId :: Control.Monad.Fail.MonadFail m => Network.HTTP.Client.Response body -> m RequestId
getRequestId =
  maybe
    (fail $ "missing required header: " <> show lambdaRuntimeAwsRequestId)
    ( pure
    . textToRequestId
    . Data.Text.Encoding.decodeUtf8With Data.Text.Encoding.Error.lenientDecode
    )
  . lookup lambdaRuntimeAwsRequestId
  . Network.HTTP.Client.responseHeaders

lambdaRuntimeAwsRequestId :: Data.CaseInsensitive.CI Data.ByteString.ByteString
lambdaRuntimeAwsRequestId = Data.CaseInsensitive.mk . Data.Text.Encoding.encodeUtf8 $ Data.Text.pack "Lambda-Runtime-Aws-Request-Id"

type Lambda = Request -> IO Response

getLambda :: IO Lambda
getLambda = do
  handler <- System.Environment.getEnv "_HANDLER"
  case handler of
    "hello.handler" -> pure exampleLambda
    _ -> fail $ "unknown handler: " <> show handler

exampleLambda :: Lambda
exampleLambda request = pure Response
  { responseBody = requestBody request
  , responseStatus = Network.HTTP.Types.ok200
  }

-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/set-up-lambda-proxy-integrations.html#api-gateway-simple-proxy-for-lambda-input-format>
data Request = Request
  { requestBody :: ! Data.Aeson.Value
  , requestMethod :: ! Network.HTTP.Types.Method
  , requestPath :: ! Data.Text.Text
  } deriving (Eq, Show)

instance Data.Aeson.FromJSON Request where
  parseJSON = Data.Aeson.withObject "Request" $ \ object -> do
    body <- do
      x1 <- optionalJsonKey object "body"
      case x1 of
        Nothing -> pure Data.Aeson.Null
        Just x2 -> do
          isEncoded <- requiredJsonKey object "isBase64Encoded"
          either fail pure
            . Data.Aeson.eitherDecodeStrict
            . (if isEncoded then Data.ByteString.Base64.decodeLenient else id)
            $ Data.Text.Encoding.encodeUtf8 x2
    method <- fmap Data.Text.Encoding.encodeUtf8 $ requiredJsonKey object "httpMethod"
    path <- requiredJsonKey object "path"
    pure Request
      { requestBody = body
      , requestMethod = method
      , requestPath = path
      }

-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/set-up-lambda-proxy-integrations.html#api-gateway-simple-proxy-for-lambda-output-format>
data Response = Response
  { responseBody :: ! Data.Aeson.Value
  , responseStatus :: ! Network.HTTP.Types.Status
  } deriving (Eq, Show)

instance Data.Aeson.ToJSON Response where
  toJSON response = Data.Aeson.object
    -- Note that the body is JSON encoded as text. In other words:
    -- > { "body": "{\"k\":\"v\"}", ... }
    [ jsonPair "body" . Data.Aeson.Text.encodeToLazyText $ responseBody response
    , jsonPair "statusCode" . Network.HTTP.Types.statusCode $ responseStatus response
    ]

sendInvocationResponse
  :: Data.Aeson.ToJSON json
  => Network.HTTP.Client.Manager
  -> ApiUrl
  -> RequestId
  -> json
  -> IO ()
sendInvocationResponse manager apiUrl requestId =
  makeApiRequest_ manager apiUrl ("/runtime/invocation/" <> requestIdToString requestId <> "/response") . Just . Data.Aeson.encode

-- This program doesn't require MongoDB to function, but I wanted to use it
-- anyway to make sure that everything got linked correctly.
requireMongoDB :: IO ()
requireMongoDB = Control.Exception.handle printSomeException
  . Control.Monad.void
  . Database.MongoDB.connect
  $ Database.MongoDB.host mempty

-- Same story for PostgreSQL.
requirePostgreSQL :: IO ()
requirePostgreSQL = Control.Exception.handle printSomeException
  . Control.Monad.void
  $ Database.PostgreSQL.Simple.connectPostgreSQL mempty

printSomeException :: Control.Exception.SomeException -> IO ()
printSomeException = print

getManager :: IO Network.HTTP.Client.Manager
getManager = Network.HTTP.Client.newManager managerSettings

-- The AWS documentation recommends not setting a timeout on HTTP calls. That's
-- because it wants to re-use Lambdas that are already running. As far as I can
-- tell it does that by making the HTTP request to get the next invocation
-- stall indefinitely. So be careful using these manager settings for regular
-- HTTP calls.
managerSettings :: Network.HTTP.Client.ManagerSettings
managerSettings = Network.HTTP.Client.defaultManagerSettings
  { Network.HTTP.Client.managerResponseTimeout = Network.HTTP.Client.responseTimeoutNone
  }

getApiUrl :: IO ApiUrl
getApiUrl = do
  awsLambdaRuntimeApi <- System.Environment.getEnv "AWS_LAMBDA_RUNTIME_API"
  pure . textToApiUrl . Data.Text.pack $ "http://" <> awsLambdaRuntimeApi <> "/2018-06-01"

newtype ApiUrl
  = ApiUrl Data.Text.Text
  deriving (Eq, Show)

textToApiUrl :: Data.Text.Text -> ApiUrl
textToApiUrl = ApiUrl

apiUrlToText :: ApiUrl -> Data.Text.Text
apiUrlToText (ApiUrl text) = text

apiUrlToString :: ApiUrl -> String
apiUrlToString = Data.Text.unpack . apiUrlToText

makeApiRequest
  :: Network.HTTP.Client.Manager
  -> ApiUrl
  -> String
  -> Maybe Data.ByteString.Lazy.ByteString
  -> IO (Network.HTTP.Client.Response Data.ByteString.Lazy.ByteString)
makeApiRequest manager apiUrl endpoint maybeBody = do
  request <- Network.HTTP.Client.parseRequest $ apiUrlToString apiUrl <> endpoint
  Network.HTTP.Client.httpLbs request
    { Network.HTTP.Client.method = case maybeBody of
      Nothing -> Network.HTTP.Types.methodGet
      Just _ -> Network.HTTP.Types.methodPost
    , Network.HTTP.Client.requestBody = Network.HTTP.Client.RequestBodyLBS
      $ case maybeBody of
        Nothing -> mempty
        Just body -> body
    } manager

makeApiRequest_
  :: Network.HTTP.Client.Manager
  -> ApiUrl
  -> String
  -> Maybe Data.ByteString.Lazy.ByteString
  -> IO ()
makeApiRequest_ manager apiUrl endpoint maybeBody = Control.Monad.void
  $ makeApiRequest manager apiUrl endpoint maybeBody

sendInitializationError
  :: Network.HTTP.Client.Manager
  -> ApiUrl
  -> Control.Exception.SomeException
  -> IO ()
sendInitializationError manager apiUrl exception = do
  print exception
  sendError manager apiUrl "/runtime/init/error" exception

newtype RequestId
  = RequestId Data.Text.Text
  deriving (Eq, Show)

textToRequestId :: Data.Text.Text -> RequestId
textToRequestId = RequestId

requestIdToText :: RequestId -> Data.Text.Text
requestIdToText (RequestId text) = text

requestIdToString :: RequestId -> String
requestIdToString = Data.Text.unpack . requestIdToText

sendInvocationError
  :: Network.HTTP.Client.Manager
  -> ApiUrl
  -> RequestId
  -> Control.Exception.SomeException
  -> IO ()
sendInvocationError manager apiUrl requestId exception = do
  print exception
  sendError manager apiUrl ("/runtime/invocation/" <> requestIdToString requestId <> "/error") exception

sendError
  :: Control.Exception.Exception exception
  => Network.HTTP.Client.Manager
  -> ApiUrl
  -> String
  -> exception
  -> IO ()
sendError manager apiUrl endpoint =
  makeApiRequest_ manager apiUrl endpoint
    . Just
    . Data.Aeson.encode
    . Data.Aeson.object
    . pure
    . jsonPair "error"
    . Control.Exception.displayException

jsonPair :: (Data.Aeson.ToJSON v, Data.Aeson.KeyValue kv) => String -> v -> kv
jsonPair key value = Data.Text.pack key Data.Aeson..= value

requiredJsonKey :: Data.Aeson.FromJSON a => Data.Aeson.Object -> String -> Data.Aeson.Types.Parser a
requiredJsonKey o k = o Data.Aeson..: Data.Text.pack k

optionalJsonKey :: Data.Aeson.FromJSON a => Data.Aeson.Object -> String -> Data.Aeson.Types.Parser (Maybe a)
optionalJsonKey o k = o Data.Aeson..:? Data.Text.pack k
