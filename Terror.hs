module Terror where

import Prelude hiding (readFile, writeFile, (++))
import BString

import Codec.Picture
import Codec.Picture.Gif
import Codec.Picture.Types

main = do
   starm <- load "starmer.gif"
   robes <- load "robespierre_red.gif"  
   let gifEnc = GifEncode {
      geWidth = 880,
      geHeight = 1168,
      gePalette = Nothing,
      geBackground = Nothing,
      geLooping = LoopingForever,
      geFrames = [starm 200, robes 1, starm 200, robes 1, starm 100, robes 1, starm 200, robes 1, starm 200, robes 1, starm 100, robes 1, starm 200, robes 1, starm 200, robes 1, starm 100, robes 1, starm 50, robes 2, starm 50, robes 4, starm 50, robes 8, starm 50, robes 15, starm 50, robes 30, starm 50, robes 500]
   }
   case encodeComplexGifImage gifEnc of
      Left l -> error l
      Right b -> writeFile "redterror.gif" b

load f = do
   b <- readFile f :: IO ByteString
   case decodeGifWithPaletteAndMetadata b of
      Left err -> error $ f ++ ": " ++ err
      {-
      Right (pic1, _) -> let
         --pic = dropTransparency pic1
         PalettedRGB16 pic8 pal1 = pic1
         pala = palettedAsImage pal1
         pal = convertImage pala
         res d = GifFrame {
             gfXOffset = 0,
             gfYOffset = 0,
             gfPalette = Just pal,
             gfTransparent = Nothing,
             gfDelay = d,
             gfDisposal = DisposalRestoreBackground,
             gfPixels = pic8
         }
         in return res
      -}
      Right (TrueColorImage pic8, _) -> error $ f ++ ": TrueColor"
      Right (PalettedRGB8 pic8 pal1, _) -> let
         --pic = dropTransparency pic1
         pal = palettedAsImage pal1
         res d = GifFrame {
             gfXOffset = 0,
             gfYOffset = 0,
             gfPalette = Just pal,
             gfTransparent = Nothing,
             gfDelay = d,
             gfDisposal = DisposalRestoreBackground,
             gfPixels = pic8
         }
         in return res
      Right (PalettedRGBA8 pic8 pal1, _) -> let
         --pic = dropTransparency pic1
         pala = palettedAsImage pal1
         pal = pixelMap dropTransparency pala
         res d = GifFrame {
             gfXOffset = 0,
             gfYOffset = 0,
             gfPalette = Just pal,
             gfTransparent = Nothing,
             gfDelay = d,
             gfDisposal = DisposalRestoreBackground,
             gfPixels = pic8
         }
         in return res
      Right (PalettedY8 pic8 pal1, _) -> let
         --pic = dropTransparency pic1
         pala = palettedAsImage pal1
         pal = promoteImage pala
         res d = GifFrame {
             gfXOffset = 0,
             gfYOffset = 0,
             gfPalette = Just pal,
             gfTransparent = Nothing,
             gfDelay = d,
             gfDisposal = DisposalRestoreBackground,
             gfPixels = pic8
         }
         in return res
