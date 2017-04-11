module Environment (
	directory,
	ClientId(..), getClientId, cidToBs,
	ClientSecret(..), getClientSecret, csToBs,
	RedirectUri(..), getRedirectUri, ruToBs
	) where

import Prelude ((.), (<$>))

import System.IO
import System.FilePath ((</>))

import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text as Txt
import qualified Data.Text.IO as Txt

import Data.ByteString (ByteString)

directory :: FilePath
directory = "/home/tatsuya/keter/skami3/"

newtype ClientId = ClientId Text

getClientId :: IO ClientId
getClientId = ClientId . Txt.concat . Txt.lines
	<$> Txt.readFile (directory </> "clientId.txt")

cidToBs :: ClientId -> ByteString
cidToBs (ClientId t) = encodeUtf8 t

newtype ClientSecret = ClientSecret Text

getClientSecret :: IO ClientSecret
getClientSecret = ClientSecret . Txt.concat . Txt.lines
		<$> Txt.readFile (directory </> "clientSecret.txt")

csToBs :: ClientSecret -> ByteString
csToBs (ClientSecret t) = encodeUtf8 t

newtype RedirectUri = RedirectUri Text

getRedirectUri :: IO RedirectUri
getRedirectUri = RedirectUri . Txt.concat . Txt.lines
		<$> Txt.readFile (directory </> "redirectUri.txt")

ruToBs :: RedirectUri -> ByteString
ruToBs (RedirectUri t) = encodeUtf8 t
