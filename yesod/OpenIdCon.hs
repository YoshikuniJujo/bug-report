module OpenIdCon (
	yconnect,
	AccessToken(..),
	debugProfile
	) where

import Import

import Crypto.Random

import qualified Data.ByteString.Base64.URL as B64
import qualified Data.Text as Txt

import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Lazy as HML

import Network.HTTP.Simple

getNonce :: MonadRandom m => Int -> m Text
getNonce = (Txt.dropWhileEnd (== '=') . decodeUtf8 . B64.encode <$>)
	. getRandomBytes

yconnect :: ClientId -> RedirectUri -> Handler Html
yconnect (ClientId cid) (RedirectUri ruri) = do
	(state, nonce, date) <- lift
		$ (,,) <$> getNonce 256 <*> getNonce 256 <*> getCurrentTime
	_ <- runDB $ insert $ OpenIdStateNonce state nonce date
	runDB (selectList ([] :: [Filter OpenIdStateNonce]) [])
		>>= mapM_ print
	yc state nonce
	where
	yc :: MonadHandler m => Text -> Text -> m a
	yc stt nnc = redirect $
		"https://auth.login.yahoo.co.jp/yconnect/v1/authorization?" <>
		"response_type=code+id_token&" <>
		"scope=openid+profile+email&" <>
		"client_id=" <> cid <> "&" <>
		"state=" <> stt <> "&" <>
		"nonce=" <> nnc <> "&" <>
		"redirect_uri=" <> ruri <> "&" <>
		"bail=1"

newtype AccessToken = AccessToken Text

debugProfile :: AccessToken -> Handler ()
debugProfile (AccessToken at) = do
	initReq <- parseRequest $
		"https://userinfo.yahooapis.jp/yconnect/v1/attribute?schema=openid"
	let	req = setRequestHeader
			"Authorization" ["Bearer " <> encodeUtf8 at] initReq
	rBody <- getResponseBody <$> httpLBS req
	let	Just json = Aeson.decode rBody :: Maybe Aeson.Object
	mapM_ putStrLn . map showSimple . sortBy (compare `on` fst) $ HML.toList json
	where
	showSimple :: (Text, Aeson.Value) -> Text
	showSimple (k, String v) = k <> ": " <> v
	showSimple (k, v) = k <> ": " <> Txt.pack (show v)
