module Monadoc ( main, scratch ) where

import qualified Codec.Archive.Tar
import qualified Codec.Compression.GZip
import qualified Control.Exception
import qualified Control.Monad
import qualified Control.Monad.Fail
import qualified Data.Aeson
import qualified Data.Aeson.Text
import qualified Data.Aeson.Types
import qualified Data.ByteString
import qualified Data.ByteString.Base64
import qualified Data.ByteString.Lazy
import qualified Data.CaseInsensitive
import qualified Data.Map
import qualified Data.Maybe
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Data.Text.Encoding.Error
import qualified Data.Text.Lazy
import qualified Distribution.PackageDescription.Parsec
import qualified Distribution.Pretty
import qualified Distribution.Types.GenericPackageDescription
import qualified Distribution.Types.PackageDescription
import qualified Distribution.Types.PackageId
import qualified Distribution.Types.PackageName
import qualified Network.HTTP.Client
import qualified Network.HTTP.Client.TLS
import qualified Network.HTTP.Types
import qualified System.Environment
import qualified System.FilePath

-- This is just a placeholder for grabbing package descriptions from Hackage. I haven't figured out where it should live yet.
scratch :: IO ()
scratch = do
  manager <- Network.HTTP.Client.TLS.newTlsManager
  request <- Network.HTTP.Client.parseUrlThrow "https://hackage.haskell.org/01-index.tar.gz"
  response <- Network.HTTP.Client.httpLbs request manager
  mapM_ print
    . fmap
      ( \ package ->
        ( Distribution.Types.PackageName.unPackageName
        . Distribution.Types.PackageId.pkgName
        . Distribution.Types.PackageDescription.package
        . Distribution.Types.GenericPackageDescription.packageDescription
        $ package
        , Distribution.Pretty.prettyShow
        . Distribution.Types.PackageId.pkgVersion
        . Distribution.Types.PackageDescription.package
        . Distribution.Types.GenericPackageDescription.packageDescription
        $ package
        , Data.Maybe.fromMaybe "0"
        . lookup "x-revision"
        . Distribution.Types.PackageDescription.customFieldsPD
        . Distribution.Types.GenericPackageDescription.packageDescription
        $ package
        )
      )
    . fmap (\ (path, contents) -> case Distribution.PackageDescription.Parsec.parseGenericPackageDescriptionMaybe contents of
      Just package -> package
      _ -> error $ "invalid package: " <> show (path, contents))
    . Data.Maybe.mapMaybe (\ entry ->
      let path = Codec.Archive.Tar.entryPath entry
      in if System.FilePath.isExtensionOf "cabal" path
        then case Codec.Archive.Tar.entryContent entry of
          Codec.Archive.Tar.NormalFile contents _ ->
            Just (path, Data.ByteString.Lazy.toStrict contents)
          _ -> error $ "unexpected entry: " <> show entry
        else Nothing)
    . Codec.Archive.Tar.foldEntries (:) [] Control.Exception.throw
    . Codec.Archive.Tar.read
    . Codec.Compression.GZip.decompress
    . Network.HTTP.Client.responseBody
    $ response

-- <https://docs.aws.amazon.com/lambda/latest/dg/runtimes-custom.html>
-- <https://docs.aws.amazon.com/lambda/latest/dg/runtimes-api.html>
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/set-up-lambda-proxy-integrations.html>
main :: IO ()
main = do
  putStrLn "Starting up ..."
  manager <- getManager
  apiUrl <- getApiUrl

  Control.Exception.handle (sendInitializationError manager apiUrl) $ do
    lambda <- getLambda

    Control.Monad.forever $ do
      putStrLn "Getting invocation ..."
      invocation <- getInvocation manager apiUrl
      requestId <- getRequestId invocation
      traceId <- getTraceId invocation
      withEnv "_X_AMZN_TRACE_ID" (traceIdToString traceId) $ do
        request <- either fail pure
          . Data.Aeson.eitherDecode
          $ Network.HTTP.Client.responseBody invocation

        Control.Exception.handle (sendInvocationError manager apiUrl requestId) $ do
          response <- lambda request
          sendInvocationResponse manager apiUrl requestId response
          putStrLn "Finished invocation."

withEnv :: String -> String -> IO a -> IO a
withEnv name value action = do
  maybeValue <- System.Environment.lookupEnv name
  Control.Exception.bracket_
    (System.Environment.setEnv name value)
    (case maybeValue of
      Nothing -> System.Environment.unsetEnv name
      Just oldValue -> System.Environment.setEnv name oldValue)
    action

getInvocation
  :: Network.HTTP.Client.Manager
  -> ApiUrl
  -> IO (Network.HTTP.Client.Response Data.ByteString.Lazy.ByteString)
getInvocation manager apiUrl =
  makeApiRequest manager apiUrl "/runtime/invocation/next" Nothing

getRequestId
  :: Control.Monad.Fail.MonadFail m
  => Network.HTTP.Client.Response body
  -> m RequestId
getRequestId = either fail (pure . textToRequestId) . getHeader "Lambda-Runtime-Aws-Request-Id"

getTraceId
  :: Control.Monad.Fail.MonadFail m
  => Network.HTTP.Client.Response body
  -> m TraceId
getTraceId = either fail (pure . textToTraceId) . getHeader "Lambda-Runtime-Trace-Id"

getHeader
  :: String
  -> Network.HTTP.Client.Response body
  -> Either String Data.Text.Text
getHeader name =
  maybe
    (Left $ "missing required header: " <> show name)
    (Right . fromUtf8)
  . lookup
    ( Data.CaseInsensitive.mk
    . toUtf8
    $ Data.Text.pack name
    )
  . Network.HTTP.Client.responseHeaders

type Lambda = Request -> IO Response

getLambda :: IO Lambda
getLambda = do
  handler <- System.Environment.getEnv "_HANDLER"
  case handler of
    "monadoc.main" -> pure exampleLambda
    _ -> fail $ "unknown handler: " <> show handler

exampleLambda :: Lambda
exampleLambda request = pure Response
  { responseBody = Just
    . Data.Text.Lazy.toStrict
    . Data.Aeson.Text.encodeToLazyText
    $ Data.Aeson.object
      [ jsonPair "body"
        . (if requestIsBase64Encoded request
          then fmap (fromUtf8 . Data.ByteString.Base64.decodeLenient . toUtf8)
          else id)
        $ requestBody request
      , jsonPair "httpMethod" $ requestHttpMethod request
      , jsonPair "isBase64Encoded" $ requestIsBase64Encoded request
      , jsonPair "multiValueHeaders" $ requestMultiValueHeaders request
      , jsonPair "multiValueQueryStringParameters" $ requestMultiValueQueryStringParameters request
      , jsonPair "path" $ requestPath request
      ]
  , responseIsBase64Encoded = False
  , responseMultiValueHeaders = Data.Map.empty
  , responseStatusCode = Network.HTTP.Types.ok200
  }

toUtf8 :: Data.Text.Text -> Data.ByteString.ByteString
toUtf8 = Data.Text.Encoding.encodeUtf8

fromUtf8 :: Data.ByteString.ByteString -> Data.Text.Text
fromUtf8 = Data.Text.Encoding.decodeUtf8With Data.Text.Encoding.Error.lenientDecode

-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/set-up-lambda-proxy-integrations.html#api-gateway-simple-proxy-for-lambda-input-format>
data Request = Request
  { requestBody :: Maybe Data.Text.Text
  , requestHttpMethod :: Data.Text.Text
  , requestIsBase64Encoded :: Bool
  , requestMultiValueHeaders :: Data.Map.Map Data.Text.Text [Data.Text.Text]
  , requestMultiValueQueryStringParameters :: Maybe (Data.Map.Map Data.Text.Text [Data.Text.Text])
  , requestPath :: Data.Text.Text
  } deriving (Eq, Show)

instance Data.Aeson.FromJSON Request where
  parseJSON = Data.Aeson.withObject "Request" $ \ object -> do
    body <- optionalJsonKey object "body"
    httpMethod <- requiredJsonKey object "httpMethod"
    isBase64Encoded <- requiredJsonKey object "isBase64Encoded"
    multiValueHeaders <- requiredJsonKey object "multiValueHeaders"
    multiValueQueryStringParameters <- optionalJsonKey object "multiValueQueryStringParameters"
    path <- requiredJsonKey object "path"
    pure Request
      { requestBody = body
      , requestHttpMethod = httpMethod
      , requestIsBase64Encoded = isBase64Encoded
      , requestMultiValueHeaders = multiValueHeaders
      , requestMultiValueQueryStringParameters = multiValueQueryStringParameters
      , requestPath = path
      }

-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/set-up-lambda-proxy-integrations.html#api-gateway-simple-proxy-for-lambda-output-format>
data Response = Response
  { responseBody :: Maybe Data.Text.Text
  , responseIsBase64Encoded :: Bool
  , responseMultiValueHeaders :: Data.Map.Map Data.Text.Text [Data.Text.Text]
  , responseStatusCode :: Network.HTTP.Types.Status
  } deriving (Eq, Show)

instance Data.Aeson.ToJSON Response where
  toJSON response = Data.Aeson.object
    [ jsonPair "body" $ responseBody response
    , jsonPair "isBase64Encoded" $ responseIsBase64Encoded response
    , jsonPair "multiValueHeaders" $ responseMultiValueHeaders response
    , jsonPair "statusCode" . Network.HTTP.Types.statusCode $ responseStatusCode response
    ]

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

newtype RequestId
  = RequestId Data.Text.Text
  deriving (Eq, Show)

textToRequestId :: Data.Text.Text -> RequestId
textToRequestId = RequestId

requestIdToText :: RequestId -> Data.Text.Text
requestIdToText (RequestId text) = text

requestIdToString :: RequestId -> String
requestIdToString = Data.Text.unpack . requestIdToText

newtype TraceId
  = TraceId Data.Text.Text
  deriving (Eq, Show)

textToTraceId :: Data.Text.Text -> TraceId
textToTraceId = TraceId

traceIdToText :: TraceId -> Data.Text.Text
traceIdToText (TraceId text) = text

traceIdToString :: TraceId -> String
traceIdToString = Data.Text.unpack . traceIdToText

sendInvocationResponse
  :: Data.Aeson.ToJSON json
  => Network.HTTP.Client.Manager
  -> ApiUrl
  -> RequestId
  -> json
  -> IO ()
sendInvocationResponse manager apiUrl requestId =
  makeApiRequest_
      manager
      apiUrl
      ("/runtime/invocation/" <> requestIdToString requestId <> "/response")
    . Just
    . Data.Aeson.encode

sendInitializationError
  :: Network.HTTP.Client.Manager
  -> ApiUrl
  -> Control.Exception.SomeException
  -> IO ()
sendInitializationError manager apiUrl exception = do
  print exception
  sendError manager apiUrl "/runtime/init/error" exception

sendInvocationError
  :: Network.HTTP.Client.Manager
  -> ApiUrl
  -> RequestId
  -> Control.Exception.SomeException
  -> IO ()
sendInvocationError manager apiUrl requestId exception = do
  print exception
  sendError
    manager
    apiUrl
    ("/runtime/invocation/" <> requestIdToString requestId <> "/error")
    exception

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

makeApiRequest_
  :: Network.HTTP.Client.Manager
  -> ApiUrl
  -> String
  -> Maybe Data.ByteString.Lazy.ByteString
  -> IO ()
makeApiRequest_ manager apiUrl endpoint =
  Control.Monad.void . makeApiRequest manager apiUrl endpoint

makeApiRequest
  :: Network.HTTP.Client.Manager
  -> ApiUrl
  -> String
  -> Maybe Data.ByteString.Lazy.ByteString
  -> IO (Network.HTTP.Client.Response Data.ByteString.Lazy.ByteString)
makeApiRequest manager apiUrl endpoint maybeBody = do
  request <- Network.HTTP.Client.parseRequest
    $ apiUrlToString apiUrl <> endpoint
  Network.HTTP.Client.httpLbs request
    { Network.HTTP.Client.method =
      case maybeBody of
        Nothing -> Network.HTTP.Types.methodGet
        Just _ -> Network.HTTP.Types.methodPost
    , Network.HTTP.Client.requestBody =
      Network.HTTP.Client.RequestBodyLBS
        $ Data.Maybe.fromMaybe mempty maybeBody
    } manager

jsonPair
  :: (Data.Aeson.ToJSON json, Data.Aeson.KeyValue pair)
  => String
  -> json
  -> pair
jsonPair key value = Data.Text.pack key Data.Aeson..= value

requiredJsonKey
  :: Data.Aeson.FromJSON json
  => Data.Aeson.Object
  -> String
  -> Data.Aeson.Types.Parser json
requiredJsonKey object key = object Data.Aeson..: Data.Text.pack key

optionalJsonKey
  :: Data.Aeson.FromJSON json
  => Data.Aeson.Object
  -> String
  -> Data.Aeson.Types.Parser (Maybe json)
optionalJsonKey object key = object Data.Aeson..:? Data.Text.pack key
