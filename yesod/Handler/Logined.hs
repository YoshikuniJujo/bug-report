module Handler.Logined where

import Import hiding ((==.), delete)
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)
import Text.Julius (RawJS (..))

import Network.HTTP.Simple

import qualified Data.ByteString.Char8 as BSC

import qualified Data.ByteString.Base64.URL as B64

import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Lazy as HML

import qualified Data.Text as Txt
import qualified Data.ByteString.Lazy as LBS

import Crypto.MAC.HMAC (HMAC, hmac, hmacGetDigest)
import Crypto.Hash.Algorithms (SHA256)

import Data.ByteArray

import Database.Esqueleto

import OpenIdCon

-- Define our data that will be used for creating the form.
data FileForm = FileForm
    { fileInfo :: FileInfo
    , fileDescription :: Text
    }

getLoginedR :: Handler Html
getLoginedR = do
	cs <- (\c s -> (,) <$> c <*> s)
		<$> lookupGetParam "code" <*> lookupGetParam "state"
	yourId <- maybe (return "unknown") (uncurry logined) cs
	showPage yourId

logined :: Text -> Text -> Handler Text
logined code state = do
	(clientId, clientSecret, redirectUri) <- lift $ (,,) 
		<$> getClientId <*> getClientSecret <*> getRedirectUri
	sn0 <- runDB . select . from $ \sn -> do
		where_ $ sn ^. OpenIdStateNonceState ==. val state
		return (
			sn ^. OpenIdStateNonceState,
			sn ^. OpenIdStateNonceNonce )
	(s0, n0) <- case sn0 of
		[(Value s, Value n)] -> return (s, n)
		_ -> error "BAD STATE"

	initReq <-
		parseRequest "https://auth.login.yahoo.co.jp/yconnect/v1/token"
	let	req = foldr (uncurry setRequestHeader)
			initReq { method = "POST" } [
			("Content-Type", ["application/x-www-form-urlencoded"]),
			basicAuthentication clientId clientSecret ]
		req' = setRequestBody (RequestBodyBS $
			"grant_type=authorization_code&" <>
			"code=" <> encodeUtf8 code <> "&" <>
			"redirect_uri=" <> ruToBs redirectUri) req
	rBody <- getResponseBody <$> httpLBS req'

	let	Just resp = Aeson.decode rBody :: Maybe Aeson.Object
	let	Just (String at) = HML.lookup "access_token" resp
		Just (String it) = HML.lookup "id_token" resp
		[hd, pl, sg] = Txt.splitOn "." it
	print $ keys resp
	let	[Just hdd, Just pld] = map
			((Aeson.decode :: LBS.ByteString -> Maybe Aeson.Object)
				. LBS.fromStrict
				. either (error . ("B64.decode error " ++) . show) id
				. B64.decode . encodeUtf8)
			[padding hd, padding pl]
	print hdd
	print pld
	print $ HML.lookup "user_id" pld
	let	Just (String n1) = lookup "nonce" pld
	when (n1 /= n0) $ error "BAD NONCE"
	runDB . delete . from $ \sc -> do
		where_ $ sc ^. OpenIdStateNonceState ==. val s0
	let sg1	= fst . BSC.spanEnd (== '=') . hmacSha256 (csToBs clientSecret)
		$ encodeUtf8 hd <> "." <> encodeUtf8 pl
	when (sg1 /= encodeUtf8 sg) $ error "BAD SIGNATURE"

	debugProfile $ AccessToken at
--	return "USER ID"
	return . fromMaybe "Unknown" $ unstring =<< HML.lookup "user_id" pld


unstring :: Aeson.Value -> Maybe Text
unstring (String t) = Just t
unstring

showPage :: Text -> Handler Html
showPage yid = do
	(formWidget, formEnctype) <- generateFormPost sampleForm
	let	yourId = yid
		submission = Nothing :: Maybe FileForm
		handlerName = "getHomeR" :: Text
	defaultLayout $ do
		let (commentFormId, commentTextareaId, commentListId) = commentIds
		aDomId <- newIdent
		setTitle "Welcome To Skami3!"
		$(widgetFile "homepage")

basicAuthentication ::
	IsString s => ClientId -> ClientSecret -> (s, [ByteString])
basicAuthentication cid cs = ("Authorization", ["Basic " <> mkClientIdSecret])
	where mkClientIdSecret = B64.encode $ cidToBs cid <> ":" <> csToBs cs

hmacSha256 :: ByteString -> ByteString -> ByteString
hmacSha256 s d = B64.encode . convert $ hmacGetDigest (hmac s d :: HMAC SHA256)

padding :: Text -> Text
padding t = t <> Txt.replicate (3 - (Txt.length t - 1) `mod` 4) "="

sampleForm :: Form FileForm
sampleForm = renderBootstrap3 BootstrapBasicForm $ FileForm
    <$> fileAFormReq "Choose a file"
    <*> areq textField textSettings Nothing
    -- Add attributes like the placeholder and CSS classes.
    where textSettings = FieldSettings
            { fsLabel = "What's on the file?"
            , fsTooltip = Nothing
            , fsId = Nothing
            , fsName = Nothing
            , fsAttrs =
                [ ("class", "form-control")
                , ("placeholder", "File description")
                ]
            }

commentIds :: (Text, Text, Text)
commentIds = ("js-commentForm", "js-createCommentTextarea", "js-commentList")
