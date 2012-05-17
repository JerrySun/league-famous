{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE OverloadedStrings #-}

module Thumbnail where

import Network.HTTP.Conduit

import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.ByteString as B
import Data.Conduit
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Binary as CB
import Data.Void (Void)
import Data.Conduit.ImageSize
import Network.HTTP.Types (ResponseHeaders)
import qualified Data.CaseInsensitive as CI
import Control.Arrow (first)
import qualified Data.Text as T
import qualified Data.Text.Read
import Control.Monad.Trans.Error
import Control.Monad.Trans (lift)
import Control.Monad (mzero, when)
import Data.Maybe (fromMaybe, isJust)
import qualified Graphics.GD.ByteString.Lazy as GD

data ThumbResult = ThumbSuccess | ThumbFail deriving (Show)

type Url = String

readDec :: Integral i => String -> Maybe i
readDec s =
    case Data.Text.Read.decimal $ T.pack s of
        Right (i, t)
            | T.null t -> Just i
        _ -> Nothing

contentLength :: ResponseHeaders -> Maybe Int
contentLength hs = lookup "content-length" hs >>= readDec . B8.unpack

data ImgType = Png | Jpeg | Gif

contentImgType :: ResponseHeaders -> Maybe ImgType
contentImgType hs = lookup "content-type" hs >>= match
    where match "image/gif" = Just Gif
          match "image/jpeg" = Just Jpeg
          match "image/png" = Just Png
          match x = Nothing

lbsSink :: Monad m => Pipe B.ByteString Void m BL.ByteString
lbsSink = fmap BL.fromChunks CL.consume

whenJust ::  Monad m => (t -> Bool) -> m () -> Maybe t -> m ()
whenJust test action maybeThing = 
    case maybeThing of
        Nothing -> return ()
        Just x -> when (test x) action

data ThumbError = InvalidUrl | TooLarge | NotImage | ThumbError deriving (Show)

instance Error ThumbError where
    noMsg = ThumbError

fetchImage :: Int -> Url -> ErrorT ThumbError IO (ImgType, BL.ByteString)
fetchImage maxBytes url = do
    let specialManager = withManager :: (Manager -> ResourceT (ErrorT ThumbError IO) a) -> (ErrorT ThumbError IO) a
    specialManager $ \manager -> do
        request <- lift $ maybe (throwError InvalidUrl) return $ parseUrl url
        Response _ _ headers src <- http request manager
        lift $ whenJust (> maxBytes) (throwError TooLarge) $ contentLength headers
        imgType <- lift $ maybe (throwError NotImage) return $ contentImgType headers
        (reSrc, part) <- src $$+ CB.isolate maxBytes =$ lbsSink
        case reSrc of
            Done Nothing () -> return (imgType, part)
            _ -> lift $ (throwError TooLarge)

thumbnail :: Int -> Url -> IO (Either ThumbError ThumbResult)
thumbnail maxBytes url = runErrorT $ do
    (imageType, imageData) <- fetchImage maxBytes url
    image <- lift $ loadImageByteString imageType imageData
    
    return ThumbSuccess
    
loadImageByteString Png = GD.loadPngByteString
loadImageByteString Gif = GD.loadGifByteString
loadImageByteString Jpeg = GD.loadJpegByteString

consumeEither ::  (Monad m, Error e) => Either e a -> ErrorT e m a
consumeEither e = case e of
    Right x -> return x
    Left y -> throwError y
