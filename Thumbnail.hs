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
import Network.HTTP.Types (ResponseHeaders)
import qualified Data.CaseInsensitive as CI
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
import Crypto.Hash.Tiger (hashlazy)
import qualified Data.ByteString.Base64.URL as B64
import System.Directory (createDirectoryIfMissing)
import System.FilePath (dropFileName)

data ThumbResult = ThumbSuccess ImageType B.ByteString
                 | ThumbFail deriving (Show)

type Url = String

data ImageType = Png | Jpeg | Gif deriving (Show)

data ThumbError = InvalidUrl | TooLarge | NotImage | ThumbError deriving (Show)

instance Error ThumbError where
    noMsg = ThumbError

data ThumbConfig = ThumbConfig { maxBytes :: Int
                               , hashLength :: Int
                               , saveRoot :: FilePath
                               , makeName :: ImageType -> B.ByteString -> [Int] -> [FilePath]
                               }

thumbDefault = ThumbConfig { maxBytes = 10 * 1024 * 1024
                           , hashLength = 12
                           , saveRoot = "static/thumbs/"
                           , makeName = makeNameSplit 2
                           }


thumbnail :: ThumbConfig -> Url -> IO (Either ThumbError ThumbResult)
thumbnail conf url = runErrorT $ do
    (imageType, imageData) <- fetchImage (maxBytes conf) url
    let imageHash = B.take (hashLength conf) . B64.encode . hashlazy $ imageData
    image <- lift $ loadImage imageType imageData
    (width, height) <- lift $ GD.imageSize image
    small <- lift $ GD.resizeImage (100) (100 * height `div` width) image
    let [path] = map (saveRoot conf <>) $ makeName conf imageType imageHash [100] 
    lift $ blazePath path
    lift $ saveImage imageType path small
    
    return $ ThumbSuccess imageType imageHash

fetchImage :: Int -> Url -> ErrorT ThumbError IO (ImageType, BL.ByteString)
fetchImage maxBytes url = do
    withManager $ \manager -> do
        request <- lift $ maybe (throwError InvalidUrl) return $ parseUrl url
        Response _ _ headers src <- http request manager
        lift $ whenJust (> maxBytes) (throwError TooLarge) $ contentLength headers
        (src', part1) <- src $$+ CB.isolate 8 =$ lbsSink
        imgType <- lift $ maybe (throwError NotImage) return $ magicDetect part1
        (reSrc, part2) <- src' $$+ CB.isolate (maxBytes - 8) =$ lbsSink
        case reSrc of
            Done Nothing () -> return (imgType, part1 <> part2)
            _ -> lift $ throwError TooLarge

blazePath = createDirectoryIfMissing True . dropFileName

makeNameSplit :: Int -> ImageType -> B.ByteString -> [Int] -> [FilePath]
makeNameSplit k imageType h sizes = map name sizes
    where name size = pre <> "/" <> suff ++ "." ++ show size ++ ext imageType
          (pre, suff) = splitAt k . B8.unpack $ h
          ext Png = ".png"
          ext Gif = ".gif"
          ext Jpeg = ".jpg"

lbsSink :: Monad m => Pipe B.ByteString Void m BL.ByteString
lbsSink = fmap BL.fromChunks CL.consume

contentLength :: ResponseHeaders -> Maybe Int
contentLength hs = lookup "content-length" hs >>= readDec . B8.unpack

magicDetect :: BL.ByteString -> Maybe ImageType
magicDetect bs = match magicGif Gif <|> match magicJpeg Jpeg
                 <|> match magicPng Png
    where match theMagic = toMaybe (theMagic `BL.isPrefixOf` bs)
          magicGif = BL8.pack "GIF8"
          magicJpeg = BL.pack [0xFF, 0xD8]
          magicPng = BL.pack [0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a]

loadImage :: ImageType -> BL.ByteString -> IO GD.Image
loadImage Png = GD.loadPngByteString
loadImage Gif = GD.loadGifByteString
loadImage Jpeg = GD.loadJpegByteString

saveImage ::  ImageType -> FilePath -> GD.Image -> IO ()
saveImage Png = GD.savePngFile
saveImage Gif = GD.saveGifFile
saveImage Jpeg = GD.saveJpegFile 90

readDec :: Integral i => String -> Maybe i
readDec s =
    case Data.Text.Read.decimal $ T.pack s of
        Right (i, t)
            | T.null t -> Just i
        _ -> Nothing

toMaybe :: Bool -> a -> Maybe a
toMaybe False _ = Nothing
toMaybe True  x = Just x

whenJust ::  Monad m => (t -> Bool) -> m () -> Maybe t -> m ()
whenJust test action maybeThing = 
    case maybeThing of
        Nothing -> return ()
        Just x -> when (test x) action

