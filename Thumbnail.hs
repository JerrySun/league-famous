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
import Control.Exception (catch, SomeException (..))
import Prelude hiding (catch)
import Control.Applicative ((<|>))
import Data.Monoid ((<>))

data ThumbResult = ThumbSuccess ImageType | ThumbFail deriving (Show)

type Url = String

readDec :: Integral i => String -> Maybe i
readDec s =
    case Data.Text.Read.decimal $ T.pack s of
        Right (i, t)
            | T.null t -> Just i
        _ -> Nothing

contentLength :: ResponseHeaders -> Maybe Int
contentLength hs = lookup "content-length" hs >>= readDec . B8.unpack

data ImageType = Png | Jpeg | Gif deriving (Show)


toMaybe :: Bool -> a -> Maybe a
toMaybe False _ = Nothing
toMaybe True  x = Just x

magicDetect :: BL.ByteString -> Maybe ImageType
magicDetect bs = match magicGif Gif <|> match magicJpeg Jpeg <|> match magicPng Png
    where match theMagic = toMaybe (theMagic `BL.isPrefixOf` bs)
          magicGif = BL8.pack "GIF8"
          magicJpeg = BL.pack [0xFF, 0xD8]
          magicPng = BL.pack [0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a]

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

fetchImage :: Int -> Url -> ErrorT ThumbError IO (ImageType, BL.ByteString)
fetchImage maxBytes url = do
    let specialManager = withManager :: (Manager -> ResourceT (ErrorT ThumbError IO) a) -> (ErrorT ThumbError IO) a
    specialManager $ \manager -> do
        request <- lift $ maybe (throwError InvalidUrl) return $ parseUrl url
        Response _ _ headers src <- http request manager
        lift $ whenJust (> maxBytes) (throwError TooLarge) $ contentLength headers
        (src', part1) <- src $$+ CB.isolate 8 =$ lbsSink
        imgType <- lift $ maybe (throwError NotImage) return $ magicDetect part1
        (reSrc, part2) <- src' $$+ CB.isolate (maxBytes - 8) =$ lbsSink
        case reSrc of
            Done Nothing () -> return (imgType, part1 <> part2)
            _ -> lift $ throwError TooLarge

thumbnail :: Int -> Url -> IO (Either ThumbError ThumbResult)
thumbnail maxBytes url = runErrorT $ do
    (imageType, imageData) <- fetchImage maxBytes url
    image <- lift $ loadImage imageType imageData
    (width, height) <- lift $ GD.imageSize image
    small <- lift $ GD.resizeImage (width `div` 5) (height `div` 5) image
    lift $ saveImage imageType "thumbnail" small
    
    return $ ThumbSuccess imageType

skip ::  IO a -> IO a -> IO a
skip x y = x `catch` specialConst y
    where specialConst = const :: a -> SomeException -> a 

loadImage :: ImageType -> BL.ByteString -> IO GD.Image
loadImage Png = GD.loadPngByteString
loadImage Gif = GD.loadGifByteString
loadImage Jpeg = GD.loadJpegByteString

saveImage ::  ImageType -> FilePath -> GD.Image -> IO ()
saveImage Png = GD.savePngFile
saveImage Gif = GD.saveGifFile
saveImage Jpeg = GD.saveJpegFile 90


consumeEither ::  (Monad m, Error e) => Either e a -> ErrorT e m a
consumeEither e = case e of
    Right x -> return x
    Left y -> throwError y
