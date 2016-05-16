{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators   #-}

module Main (main) where

import Codec.Picture
import Codec.Picture.Types -- to work with mutable images later
import Control.Monad
import Data.Array.Repa (Array, DIM1, DIM2, U, D, Z (..), (:.)(..), (!))
import System.Environment (getArgs)
import System.FilePath (replaceExtension)
import qualified Data.Array.Repa as R -- for Repa

-- Image format conversion with Juicy Pixels

data ImgFormat = Bmp | Jpg | Png | Tiff

main0 :: IO ()
main0 = do
  [ext, path] <- getArgs
  case fromExt ext of
    Nothing -> putStrLn "Sorry, I don't know such format!"
    Just fmt -> convertImg fmt path

convertImg
  :: ImgFormat         -- ^ Format of resulting image
  -> FilePath          -- ^ Where to get source image
  -> IO ()
convertImg fmt path = do
  eimg <- readImage path
  case eimg of
    Left err -> putStrLn ("Could not read image: " ++ err)
    Right img ->
      (case fmt of -- select saving function
        Bmp  -> saveBmpImage
        Jpg  -> saveJpgImage 100
        Png  -> savePngImage
        Tiff -> saveTiffImage)
      (replaceExtension path (toExt fmt)) -- replace file extension
      img -- pass it 'DynamicImage' to save

toExt :: ImgFormat -> String
toExt Bmp      = "bmp"
toExt Jpg      = "jpeg"
toExt Png      = "png"
toExt Tiff     = "tiff"

fromExt :: String -> Maybe ImgFormat
fromExt "bmp"  = Just Bmp
fromExt "jpeg" = Just Jpg
fromExt "png"  = Just Png
fromExt "tiff" = Just Tiff
fromExt _      = Nothing

-- Image rotation with Juicy Pixels

main1 :: IO ()
main1 = do
  [path, path'] <- getArgs
  eimg <- readImage path
  case eimg of
    Left err -> putStrLn ("Could not read image: " ++ err)
    Right (ImageRGB8 img) -> do
      img' <- rotateImg img
      savePngImage path' (ImageRGB8 img')
    Right _ -> putStrLn "Unexpected pixel format"

rotateImg :: Image PixelRGB8 -> IO (Image PixelRGB8)
rotateImg img@Image {..} = do
  mimg <- newMutableImage imageWidth imageHeight
  forM_ [(x,y) | x <- [0..imageWidth-1], y <- [0..imageHeight-1]] $ \(x,y) ->
    writePixel mimg (imageWidth - x - 1) (imageHeight - y - 1) (pixelAt img x y)
  freezeImage mimg

-- Image generation with Juicy Pixels

main2 :: IO ()
main2 = do
  [path] <- getArgs
  savePngImage path generateImg

generateImg :: DynamicImage
generateImg = ImageRGB8 (generateImage originalFnc 1200 1200)

originalFnc :: Int -> Int -> PixelRGB8
originalFnc x y =
  let (q, r) = x `quotRem` max 3 y
      s      = fromIntegral . min 0xff
  in PixelRGB8 (s q) (s r) (s (q + r + 30))

-- Helpers for conversion between JuicyPixels and Repa arrays

type RGB8 = (Pixel8, Pixel8, Pixel8)

-- | Produce delayed Repa array from image with true color pixels.

fromImage :: Image PixelRGB8 -> Array D DIM2 RGB8
fromImage img@Image {..} =
  R.fromFunction
    (Z :. imageWidth :. imageHeight)
    (\(Z :. x :. y) ->
       let (PixelRGB8 r g b) = pixelAt img x y
       in (r, g, b))

-- | Get image with true color pixels from manifest Repa array.

toImage :: Array U DIM2 RGB8 -> Image PixelRGB8
toImage a = generateImage gen width height
  where
    Z :. width :. height = R.extent a
    gen x y =
      let (r,g,b) = a ! (Z :. x :. y)
      in PixelRGB8 r g b

-- Image rotation with Repa

main3 :: IO ()
main3 = do
  [path, path'] <- getArgs
  eimg <- readImage path
  case eimg of
    Left err -> putStrLn ("Could not read image: " ++ err)
    Right (ImageRGB8 img) -> do
      computed <- (R.computeUnboxedP . rotateImgRepa . fromImage) img
      (savePngImage path' . ImageRGB8 . toImage) computed
    Right _ -> putStrLn "Unexpected pixel format"

rotateImgRepa :: R.Source r e => Array r DIM2 e -> Array D DIM2 e
rotateImgRepa g = R.backpermute e remap g
  where
    e@(Z :. width :. height) = R.extent g
    remap (Z :. x :. y) = Z :. width - x - 1 :. height - y - 1
    {-# INLINE remap #-}

-- Generating an image with Repa

main4 :: IO ()
main4 = do
  [path] <- getArgs
  img    <- R.computeUnboxedP generateImgRepa
  (savePngImage path . ImageRGB8 . toImage) img

generateImgRepa :: Array D DIM2 RGB8
generateImgRepa = R.fromFunction (Z :. 600 :. 600) originalFnc'

originalFnc' :: (Z :. Int :. Int) -> RGB8
originalFnc' (Z :. x :. y) =
  let (q, r) = x `quotRem` max 3 y
      s      = fromIntegral . min 0xff
  in (s q, s r, s (q + r + 30))

main :: IO ()
main = main4
