-- Copyright 2025 Brett Curtis
{-# LANGUAGE LexicalNegation #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use when" #-}
{-# LANGUAGE BlockArguments #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Use infix" #-}
{-# HLINT ignore "Parenthesize unary negation" #-}

module Atlas where

import Favs hiding (on)
import HTML
import MHashDynamic2
import Matrix hiding ((*), (+), (-))
import MyPretty2

import Prelude hiding ((<*>))

import Data.Bits
import Data.Map qualified as M

import Codec.Picture
import Codec.Picture.Types
import Data.List
import Data.IORef
import System.Directory
import System.Process

import Graphics.UI.Gtk
import Graphics.Rendering.Cairo
import Graphics.Rendering.Cairo qualified as Cairo

-- import Graphics.UI.Gtk.General.Structs

--atlas = "/home/brett/code/html/atlas/"
--atlas = "/home/brett/Documents/Info/Atlas/"
atlas = "/home/brett/code/html/atlas/"

main = main2


fmt  n x = let s = show $ abs x in (if x < 0 then "-" else "+") ++ drop (length s) (replicate n '0') ++ s
fmt1 n x = let s = show $ abs x in (if x < 0 then "-" else "" ) ++ drop (length s) (replicate n '0') ++ s
{-
newtype Degrees a = Degrees a

instance Num a => Num (Degrees a) where
   Degrees a + Degrees b = Degrees (a + b)
   Degrees a - Degrees b = Degrees (a - b)
   Degrees a * Degrees b = Degrees (a * b)

instance Fractional a => Fractional (Degrees a) where

instance Floating a => Floating (Degrees a) where
   sin (Degrees a) = sin (a / 180 * pi)
-}
--sin a = Prelude.sin (a / 180 * pi)
{-
makepages = mapM_ (\n -> writeFile ("page"++fmt n++".html") $
    formatHtml $ html [] $ gridH [
         "<html>",
         "    <body>",
         "         <table>",
         "             <tr><td><img src='pic"++fmt n++".png'></td><td><img src='pic"++fmt (n+1)++".png'></td></tr>",
         "         </table>",
         "    </body>",
         "</html>"]) [0, 2..434]
-}
pages =
   [ "The Solar System"
   , "The Physical World"
   , "Structure of the Earth"
   , "Shaping the Landscape"
   , "The Worlds's Oceans"
   , "The Global Climate"
   , "Life on Earth"
   , "Man's Impact on the Environment"
   , "Population & Settlement"
   , "Languages & Religion"
   , "World Health"
   , "Water Resources"
   , "The Economic System"
   , "Global Communication"
   , "The Political World"
   , "States, Borders, Conflicts & Disputes"
   , "Standard Time Zones"
   , "North America"
   , "North America Political"
   , "North America Physical"
   , "North America satellite image"
   , "Canada"
   , "Northern Canada"
   , "Western Canada"
   , "Southwest Canada"
   , "Eastern Canada"
   , "United States of America"
   , "US: NE States & Southeast Canada"
   , "US: Boston to Washington DC"
   , "US: Eastern States"
   , "US: Southeastern States"
   , "US: Texas"
   , "US: Great Lakes States"
   , "US: Great Plains States"
   , "US: Northwest States"
   , "US: Southwest States"
   , "US: California"
   , "US: Alaska & Hawaii"
   , "Northern Mexico"
   , "Southern Mexico"
   , "Central America"
   , "The Caribbean"
   , "North American City Plans"
   , "South America"
   , "South America Political"
   , "South America Physical"
   , "South America Satellite image"
   , "Northern South America"
   , "Western South America including Galapagos"
   , "Amazon Basin"
   , "Eastern Brazil"
   , "Southeast Brazil"
   , "Central South America"
   , "River Plate"
   , "Southern Argentina & Chile"
   , "Central Chlie & Argentina"
   , --    "South America City Plans",

      "Africa"
   , "Afirca Political"
   , "Africa Physical"
   , "Africa Satellite image"
   , "Northeast Africa"
   , "Northwest Africa"
   , "West Africa including Cape Verde"
   , "Central Africa"
   , "East Africa"
   , "Southern Africa including Madagascar and Comoros"
   , "South Africa"
   , "Africa City Plans"
   , "Europe"
   , "Europe Political"
   , "Europe Physical"
   , "Europe Satellite image"
   , "Scandinavia, Finland & Iceland"
   , "Southern Scandinavia"
   , "United Kingdom & Ireland"
   , "Northern Britain & Ireland"
   , "Southern Britain"
   , "The Low Countries"
   , "France"
   , "Northern France"
   , "Southern France & the Pyrenees"
   , "The Iberian Peninsula"
   , "Southern Iberia"
   , "The Italian Peninsula"
   , "The Alpine States & Northern Italy"
   , "Germany"
   , "Rhineland & Hamburg"
   , "Central Europe"
   , "Southeast Europe"
   , "Greece"
   , "Romania, Moldova & Ukraine"
   , "The Baltic States & Belarus"
   , "Russian Federation"
   , "Northern European Russia"
   , "Southern European Russia"
   , "The Mediterranean"
   , "European City Plans 1"
   , "European City Plans 2"
   , "Asia"
   , "Asia Political"
   , "Asia Physical"
   , "Asia Satellite image"
   , "Southwest Asia"
   , "Turkey & the Caucasus"
   , "The Near East"
   , "Israel & Lebanon"
   , "The Arabian Peninsula"
   , "Iran"
   , "Central & South Asia"
   , "Kazakhstan"
   , "Central Asia"
   , "Afghanistan & Pakistan"
   , "Northern India, Nepal & Bangladesh"
   , "Southern Inda & Sri Lanka"
   , "East Asia"
   , "Western China"
   , "Southeast China"
   , "Yangtze River Valley"
   , "Yellow River Valley"
   , "Northeast China"
   , "Korea & Japan"
   , "Southern Japan"
   , "Northern Japan"
   , "Southeast Asia"
   , "Mainland Southeast Asia"
   , "Western Maritime Southeast Asia"
   , "Eastern Maritime Southeast Asia"
   , "South China Sea & the Philippines"
   , "Asian City Plans"
   , "Australasia & Oceania"
   , "Australasia & Oceania Political"
   , "Australasia & Oceania Physical"
   , "Australasia & Oceania Satellite image"
   , "Australia"
   , "Southeast Australia"
   , "New Zealand"
   , "Papua New Guinea & Melanesia"
   , "Micronesia"
   , "Polynesia"
   , "Pacific Ocean"
   , "Indian Ocean"
   , "Atlantic Ocean"
   , "Antarctica"
   , "The Arctic"
   , "Geographical Comparisons"
   , "Countries of the World"
   , "Geographical Names"
   , "Index"
   , "Credits/Acknowledgements"
   ]

path = "/home/brett/Documents/Info/Atlas/"

hlink rel typ href = EmptyTag "link" [("rel", rel), ("type", typ), ("href", href)]

stylesheet = hlink "stylesheet" "text/css"

defstyle = [stylesheet "https://www.w3.org/StyleSheets/TR/W3C-REC"]

button = Tag "td" [("align", "center"), ("valign", "center")] . s . Text

iframe name src = with [("width", "100%"), ("height", "100%")] $ Tag "iframe" [("name", name), ("src", src)] []

writeHtml f b h = writeFile f $ formatH 1 $ html b h
{-
index =
   writeFile (path ++ "index.html") $
      formatH 1 $
         html defstyle $
            s $
               gridH
                  [ [with1 "width" "100%" $ gridH $ map2 (singleton . button) [["PREV", "NORTH", "NEXT"], ["WEST", "HOME", "EAST"], ["IN", "SOUTH", "OUT"]]]
                  ,
                     [ cellA [("height", "1700")] $ iframe "contents" "contents.html"
                     , cellA [("width", "91%"), ("height", "1700")] $ iframe "main" "index1.html"
                     ]
                  ]
-}
contents = writeHtml (path ++ "contents.html") defstyle $ s $ olist $ zipWith (\n p -> with [("target", "main")] $ textLink ("joined/" ++ fmt1 3 n ++ ".png") p) [8, 10 .. 294] pages

makepages = do
   setCurrentDirectory path
   mapM_ (\n -> writeFile ("pages/" ++ fmt1 3 n ++ ".html") $ formatH 1 $ html defstyle $ s $ Tag "object" [("src", "../joined/" ++ fmt1 3 n ++ ".png"), ("usemap", "#map1")] []) [2, 4 .. 434]

mv = do
   setCurrentDirectory "/home/brett/Documents/Info/Atlas"
   mapM_ (\n -> callCommand ("mv joined" ++ fmt1 3 n ++ ".png joined/" ++ fmt1 3 n ++ ".png")) [2, 4 .. 434]

join =
   mapM_
      ( \n -> do
            setCurrentDirectory "/home/brett/Documents/Info/Atlas"
            Right (ImageRGB8 left ) <- readImage ("pic" ++ fmt1 3  n      ++ ".png")
            Right (ImageRGB8 right) <- readImage ("pic" ++ fmt1 3 (n + 1) ++ ".png")
            let page = generateImage (\x y -> if x < 3131 then pixelAt left x y else pixelAt right (x - 3131) y) 6262 4312
            savePngImage ("joined" ++ fmt1 3 n ++ ".png") (ImageRGB8 page)
      )
      [2, 4 .. 434]

crossWithM :: Monad m => (a -> b -> m c) -> [a] -> [b] -> m [c]
crossWithM  f xs ys = sequence $ concat $ crossWith f xs ys
crossWithM_ f xs ys = do crossWithM f xs ys; return ()

mix4 :: Pixel8 -> Pixel8 -> Pixel8 -> Pixel8 -> Double -> Double -> Pixel8
mix4 p00 p01 p10 p11 xf yf = let
   xd0 = fromIntegral $ p10 - p00
   yd0 = fromIntegral $ p01 - p00
   xd1 = fromIntegral $ p11 - p01
   xdd = xd1 - xd0
   xd  = xd0 + xdd * yf
   in round $ fromIntegral p00 + xd * xf + yd0 * yf
   --in p00 + (p10 - p00) * xf

readPixelFrac img x y = do
   PixelRGBA8 r00 g00 b00 _ <- readPixel img (floor   x `mod` 3840) (floor   y `mod` 2160)
   PixelRGBA8 r01 g01 b01 _ <- readPixel img (floor   x `mod` 3840) (ceiling y `mod` 2160)
   PixelRGBA8 r10 g10 b10 _ <- readPixel img (ceiling x `mod` 3840) (floor   y `mod` 2160)
   PixelRGBA8 r11 g11 b11 _ <- readPixel img (ceiling x `mod` 3840) (ceiling y `mod` 2160)
   let xf = x - fromIntegral (floor x)
   let yf = y - fromIntegral (floor y)
   return $ PixelRGB8 (mix4 r00 r01 r10 r11 xf yf) (mix4 g00 g01 g10 g11 xf yf) (mix4 b00 b01 b10 b11 xf yf)

black = PixelRGB8 0 0 0

vcross [[x0, y0, z0]] [[x1, y1, z1]] = [[y0*z1-z0*y1, z0*x1-x0*z1, x0*y1-y0*x1]]

testrot rot r cam sc [[latd, longd]] = let
   lat = rofd latd
   long = rofd longd
   pos@[[x2, y2, z2]] = rot <*> ([[r * sin long * cos lat, -(r * sin lat), r * cos long * cos lat]] <-> cam)
   in [[960 + x2/z2 * sc * 0.05, 540 + y2/z2 * sc * 0.05]]

-- russia
-- sintra x=1357 y=1907 lat=38o47'57" N long=9o23'18" W
-- vagur  x=1667 y= 183 lat=61o28'31" N long=6o48'26" W
-- gelendzhik =  x=3694 lat=44.5782103  long=38.0018764


join2 = do
   setCurrentDirectory "/home/brett/Documents/Info/earth/sat3000"
   let xs = 1800
   let ys = 900
   let mu = div xs 360
   let xs2 = div xs 2
   let ys2 = div ys 2
   let xn = 12
   let yn = 7
   let r = 6371
   let r2 = r + 30000

   let lat  = rofd 50
   let long = rofd 0
   let cam = [[r2 * sin long * cos lat, -(r2 * sin lat), r2 * cos long * cos lat]]
   print cam
   let rot1 = unit cam
   let xx = unit $ vcross [[0, -1, 0]] rot1
   let yy = unit $ vcross rot1 xx
   let rot2 = xx <++> yy <++> rot1
   let rot = invMat rot2

   print (rot2, rot)

   let lat1  = rofd  61+28/60+31/3600 --44.5782103
   let long1 = rofd -(6+48/60+26/3600) --38.0018764

   let p0 = [[r * sin long1 * cos lat1, -(r * sin lat1), r * cos long1 * cos lat1]]
   let p1 = p0 <-> cam
   let pos@[[x2, y2, z2]] = rot <*> p1

   let sc = (3694 - 1920) * z2/x2
   let sc1 = (183 - 1080) * z2/y2
   print (p0, p1, pos, sc, sc1, x2/z2 * sc + 1920)
   print ()

   bigmap <- newMutableImage xs ys
   crossWithM
      ( \(latd::Double) (longd::Double) -> do
         let lat  = rofd latd
         let long = rofd longd
         let fi = fmt1 3 (100-round latd) ++ "_" ++ fmt1 3 (round longd) ++ ".png"
         lr <- readImage fi
         print fi
         let src = case lr of
                     Left l -> error l
                     Right (ImageRGBA8 i) -> i
         src1 <- thawImage src
         let cam = [[r2 * sin long * cos lat, -(r2 * sin lat), r2 * cos long * cos lat]]
         let rot1 = unit cam
         let xx = unit $ vcross [[0, -1, 0]] rot1
         let yy = unit $ vcross rot1 xx
         let rot = invMat (xx <++> yy <++> rot1)
         let t = testrot rot r cam sc

         withImageSurface FormatRGB24 1920 1080 $ \surface -> do
       -- Perform Cairo operations on the surface
            pixbuf <- pixbufNewFromFile fi
            renderWith surface $ do
               setSourcePixbuf pixbuf 0 0
               Cairo.paint
               setLineWidth 1
               setSourceRGB 1 0 1
               sphereGrid t 10 2
               setSourceRGB 0 1 1
               sphereGridPatch t (latd-15) (latd+15) (longd-15) (longd+15) 5 1
            surfaceWriteToPNG surface ("x" ++ fi)

         crossWithM (\(x1::Int) (y1::Int) -> do
            let lat1  = lat  + rofd (fromIntegral y1 / fromIntegral mu)
            let long1 = long + rofd (fromIntegral x1 / fromIntegral mu)
            let pos@[[x2, y2, z2]] = rot <*> ([[r * sin long1 * cos lat1, -(r * sin lat1), r * cos long1 * cos lat1]] <-> cam)

            --print (dofr lat, dofr long, dofr lat1, dofr long1, pos, x2/z2 * sc + 1920, y2/z2 * sc + 1080, sc)
            p <- readPixelFrac src1 (x2/z2 * sc + 1920) (y2/z2 * sc + 1080)
            writePixel bigmap (mod (900+round longd*mu+x1) 1800) (mod (450+round latd*mu+y1) 900) p)
            [-75..75] [-75..75]
      )
      [-75, -50 .. 75] [0, 30 .. 330]
   out <- freezeImage bigmap
   savePngImage "bigmap.png" $ ImageRGB8 out

gangs = do
   setCurrentDirectory "/home/brett/Pictures/Atheism/RapeGangs/Offenders/Scaled"
   files <- listDirectory "."
   r1 <- mapM (\filename -> do
      if isSuffixOf ".jpg" filename then do
         let Just ul = elemIndex '_' filename
         let l = length filename
         let filename2 = take (l-4) filename
         let filename1 = drop (ul+1) filename
         let a = read $ take 1 filename1 :: Double
         let b = read $ take 1 $ drop 2 filename1 :: Double
         let x0   | filename1 !! 1 == 'x' = a
                  | otherwise             = max a b
         let y    | filename1 !! 1 == 'x' = b
                  | otherwise             = 2
         src <- pixbufNewFromFile filename
         xs <- fromIntegral <$> pixbufGetWidth  src
         ys <- fromIntegral <$> pixbufGetHeight src
         singles <- concat <$> mapM (\y1 -> do
            let x | filename1 !! 1 == 'x' = a
                  | y1 == 0               = a
                  | otherwise             = b
            let xd = x * 200
            let yd = y * 200
            let z = max (xd / xs) (yd / ys)
            let xo = (0 - xs * z) / x
            let yo = (0 - ys * z) / y
            let xp = (xd - xs * z) / x / 2
            let yp = (yd - ys * z) / y / 2
            print (filename, xs, ys, xd, yd, z, xo, yo)
            mapM (\x1 -> do
               dest <- pixbufNew ColorspaceRgb False 8 200 200
               pixbufScale src dest 0 0 200 200 (x1 * xo + xp) (y1 * yo + yp) z z InterpHyper
               withImageSurface FormatRGB24 200 200 $ \surface -> do
                  renderWith surface $ do
                     setSourcePixbuf dest 0 0
                     Cairo.paint
                  let n | filename1 !! 1 == 'x' = x0 * y1 + x1
                        | y1 == 0               = x1
                        | otherwise             = a + x1
                  let filename3 = "out/" ++ filename2 ++ "_" ++ show (round n) ++ ".png"
                  surfaceWriteToPNG surface filename3
                  return filename3
               ) [0..(x-1)]
            ) [0..(y-1)]
         return (filename, singles)
         {-
         if filename1 !! 1 == 'x'
            then return $ a * b
            else return $ a + b
         -}
         {-
         withImageSurface FormatRGB24 (round $ x*200) (round $ y*200) $ \surface -> do
            renderWith surface $ do
               scale z z
               mapM (\x1 -> 
                  mapM (\y1 -> do
                     setSourcePixbuf pixbuf (x1 * xsm + (xsm - ss) / 2) (y1 * ysm + (ysm - ss) / 2)
                     --setSourceRGB 1 1 1
                     newPath
                     
                     moveTo ( x1   *ss) ( y1   *ss)
                     lineTo ((x1+1)*ss) ( y1   *ss)
                     lineTo ((x1+1)*ss) ((y1+1)*ss)
                     lineTo ( x1   *ss) ((y1+1)*ss)
                     lineTo ( x1   *ss) ( y1   *ss)
                     
                     --Cairo.rectangle (x*200) (y*200) (x*200+200) (y*200+200)
                     fill
                     --fill
                     ) [0..(y-1)]
                  ) [0..(x-1)]
            surfaceWriteToPNG surface $ "out/" ++ filename
         -}

      else
         return ([], [])) files
   writeFileBinary "index.html" $ lbsofs $ formatH 1 $ Tag "table" [] $ map (\(n, r2) -> Tag "tr" [] $ map (\f -> Tag "td" [] [Tag "img" [("src", f)] []]) r2) r1


gangs1 = do
   setCurrentDirectory "/home/brett/Pictures/Atheism/RapeGangs/Offenders/Scaled"
   files <- listDirectory "."
   mapM_ (\filename -> do
      if isSuffixOf ".jpg" filename then do
         let Just ul = elemIndex '_' filename
         let filename1 = drop (ul+1) filename
         let a = read $ take 1 filename1 :: Double
         let b = read $ take 1 $ drop 2 filename1 :: Double
         let x0   | filename1 !! 1 == 'x' = a
                  | otherwise = max a b
         let y = if filename1 !! 1 == 'x' then b else 2
         src <- pixbufNewFromFile filename
         xs <- fromIntegral <$> pixbufGetWidth  src
         ys <- fromIntegral <$> pixbufGetHeight src
         let xz = x0 * 200
         let yz = y  * 200
         dest <- pixbufNew ColorspaceRgb False 8 (round xz) (round yz)
         mapM_ (\y1 -> do
            let x | filename1 !! 1 == 'x' = a
                  | y1 == 0 = a
                  | otherwise = b
            let xd = x * 200
            let yd = y * 200
            let z = max (xd / xs) (yd / ys)
            let xo = (xd - xs * z) / x
            let yo = (yd - ys * z) / y
            print (filename, xs, ys, xd, yd, z, xo, yo)
            mapM_ (\x1 ->
               pixbufScale src dest (round $ x1*200) (round $ y1*200) 200 200 ((x1 + 0.5) * xo) ((y1 + 0.5) * yo) z z InterpHyper
               ) [0..(x-1)]
            ) [0..(y-1)]
         withImageSurface FormatRGB24 (round xz) (round yz) $ \surface -> do
            renderWith surface $ do
               setSourcePixbuf dest 0 0
               Cairo.paint
            surfaceWriteToPNG surface $ "out/" ++ filename

         {-
         withImageSurface FormatRGB24 (round $ x*200) (round $ y*200) $ \surface -> do
            renderWith surface $ do
               scale z z
               mapM (\x1 -> 
                  mapM (\y1 -> do
                     setSourcePixbuf pixbuf (x1 * xsm + (xsm - ss) / 2) (y1 * ysm + (ysm - ss) / 2)
                     --setSourceRGB 1 1 1
                     newPath
                     
                     moveTo ( x1   *ss) ( y1   *ss)
                     lineTo ((x1+1)*ss) ( y1   *ss)
                     lineTo ((x1+1)*ss) ((y1+1)*ss)
                     lineTo ( x1   *ss) ((y1+1)*ss)
                     lineTo ( x1   *ss) ( y1   *ss)
                     
                     --Cairo.rectangle (x*200) (y*200) (x*200+200) (y*200+200)
                     fill
                     --fill
                     ) [0..(y-1)]
                  ) [0..(x-1)]
            surfaceWriteToPNG surface $ "out/" ++ filename
         -}

      else
         return ()) files
join3 = do
   let bigmap = generateImage (\x y -> PixelRGB8 0 0 0) 6262 4312
   savePngImage "bigmap.png" $ ImageRGB8 bigmap


small =
   mapM_
      ( \n -> do
         setCurrentDirectory "/home/brett/Documents/Info/Atlas"
         Right (ImageRGB8 im) <- readImage ("joined/" ++ fmt 3 n ++ ".png")
         let page = generateImage (\x y -> pixelAt im (x * 19) (y * 19)) 320 220
         savePngImage ("small/" ++ fmt1 3 n ++ ".png") (ImageRGB8 page)
      )
      [2, 4 .. 434]

itype (ImageY8     i) = "Y8"
itype (ImageY16    i) = "Y16"
itype (ImageY32    i) = "Y32"
itype (ImageYF     i) = "YF"
itype (ImageYA8    i) = "YA8"
itype (ImageYA16   i) = "YA16"
itype (ImageRGB8   i) = "RGB8"
itype (ImageRGB16  i) = "RGB16"
itype (ImageRGBA8  i) = "RGBA8"
itype (ImageRGBA16 i) = "RGBA16"
itype (ImageRGBF   i) = "RGBF"
itype (ImageYCbCr8 i) = "YCbCr8"
itype (ImageCMYK8  i) = "CMYK8"
itype (ImageCMYK16 i) = "CMYK16"

test = do
   setCurrentDirectory "/home/brett/Documents/Info/earth/sat3000"
   files <- listDirectory "."
   mapM_ (\f -> do i <- readImage f; print (f, case i of { Left l -> l; Right r -> itype r })) files

{-
lamconOfSph lam0 phi0 phi1 phi2 rad [[lat, long]] = [[rho * sin (n * (long - lam0)), rho0 - rho * cos (n * (long - lam0))]]
 where
   rho = rad * f * cot (pi / 4 + lat / 2) ** n
   rho0 = rad * f * cot (pi / 4 + phi0 / 2) ** n
   f = cos phi1 * tan (pi / 4 + phi1 / 2) ** n / n
   n = log (cos phi1 * sec phi2) / log (tan (pi / 4 + phi2 / 2) * cot (pi / 4 + phi1 / 2))

sphOfLamcon lam0 phi0 phi1 phi2 rad [[x, y]] = [[lat, long]]
 where
   lat = 2 * acot ((rho / rad / f) ** (1 / n)) - pi / 2
   long = atan2 x (rho0 - y) / n + lam0
   rho = sqrt (x ^ 2 + (rho0 - y) ^ 2)
   rho0 = rad * f * cot (pi / 4 + phi0 / 2) ** n
   f = cos phi1 * tan (pi / 4 + phi1 / 2) ** n / n
   n = log (cos phi1 * sec phi2) / log (tan (pi / 4 + phi2 / 2) * cot (pi / 4 + phi1 / 2))

-}
rofd = ((pi / 180) *)
dofr = ((180 / pi) *)
rofd2 = ((pi / 180) |*|)
dofr2 = ((180 / pi) |*|)
rofd3 a b = [[a]] <**> rofd2 b
dofr3 a = dofr2 $ sing $ drop 1 $ only a

shift (sx, sy) (x, y) = (x + sx, y + sy)
voft (x, y) = [[x, y]]
tofv [[x, y]] = (x, y)
vec x y = [[x, y]]
affine (mat, sh) v = mat <*> v <+> sh
scalesh (scale, sh) v = scale |*| v <+> sh
scalerotsh (scale, rot, shift) v = zipWith2d (*) scale $ (rotation rot <*> v) <+> shift

movTo [[x, y]] = moveTo x y
linTo [[x, y]] = lineTo x y

lamconOfSph lam0 phi1 phi2 rad [[lat, long]] = [[rho * sin (n * (long - lam0)), rho * cos (n * (long - lam0))]]
 where
   rho = rad * f * cot (pi / 4 + lat / 2) ** n
   f = cos phi1 * tan (pi / 4 + phi1 / 2) ** n / n
   n = log (cos phi1 * sec phi2) / log (tan (pi / 4 + phi2 / 2) * cot (pi / 4 + phi1 / 2))

sphOfLamcon lam0 phi1 phi2 rad [[x, y]] = [[lat, long]]
 where
   lat = 2 * acot ((rho / rad / f) ** (1 / n)) - pi / 2
   long = atan2 x y / n + lam0
   rho = sqrt (x ^ 2 + y ^ 2)
   f = cos phi1 * tan (pi / 4 + phi1 / 2) ** n / n
   n = log (cos phi1 * sec phi2) / log (tan (pi / 4 + phi2 / 2) * cot (pi / 4 + phi1 / 2))

sec theta = 1 / cos theta
cot theta = 1 / tan theta
acot x = atan (1 / x)

lamaziOfSph1 [[lat, long]] = [[2 * cos (colat / 2), long]] where colat = lat + pi / 2

sphOfLamazi1 [[r, theta]] = [[2 * acos (r / 2) - pi / 2, theta]]

cartOfPol [[r, theta]] = [[r * sin theta, r * cos theta]]

polOfCart [[x, y]] = [[sqrt (x ^ 2 + y ^ 2), atan2 x y]]
{-
sphOfCart3 [[x, y, z]] =
   let
      r   = sqrt (x^2 + y^2 + z^2)
      lat = asin (-y/r)
-}
sphOfCart3 [[x, y, z]] = let
   long = atan2 x z
   lat  = atan2 -y (z / cos long)
   r    = sqrt (x^2 + y^2 + z^2)
   in [[r, lat, long]]


{-
cart3OfSph [[r, lat, long]] =
   let
      [[z, greenLen]] = cartOfPol [[r, lat]]
      [[x, y]] = cartOfPol [[greenLen, long]]
    in
      [[x, y, z]]
-}
cart3OfSph [[r, lat, long]] = [[r * sin long * cos lat, -(r * sin lat),   r * cos long * cos lat]]

lamaziOfSph (scale, sh, longsh) v = scalesh (scale, sh) $ cartOfPol $ lamaziOfSph1 $ v <+> vec 0 longsh
sphOfLamazi (scale, sh, longsh) v = (<-> vec 0 longsh) $ sphOfLamazi1 $ polOfCart v

lamaziCart3OfCart [[x, y]] = [[sqrt (1 - r2 / 4) * x, sqrt (1 - r2 / 4) * y, 1 - r2 / 2]]
 where
   r2 = x ^ 2 + y ^ 2

sing x = [x]
only [x] = x

lamaziCartOfCart3 q = case q of
   [[x, y, z]] -> [[sqrt (2 / (1 + z)) * x, sqrt (2 / (1 + z)) * y]]
   p -> error $ show p

uns x = case x of
   [y] -> y
   z -> error ("uns on " ++ show z)

lamazi3 :: [Double] -> [Double] -> [[Double]] -> [[Double]]
lamazi3 [rotx, rotz] [scalex, scaley, rot, xoff, yoff] d = mapcols (scalerotsh (vec scalex scaley, rot, vec xoff yoff) . lamaziCartOfCart3 . ((rotX (rofd rotx) <*> rotY (rofd rotz)) <*>) . cart3OfSph . rofd3 1) d

lamazi4 [rotx, roty] mat d = mapcols ((mat <*>) . add1s . lamaziCartOfCart3 . (rotX (rofd rotx) <*>) . (rotY (rofd roty) <*>) . cart3OfSph . rofd3 1) d

lamazi4a a b lat long = lamazi4 a b [[lat, long]]

ilamazi4 [rotx, roty] imat d = mapcols (dofr3 . sphOfCart3 . (rotY (rofd -roty) <*>) . (rotX (rofd -rotx) <*>) . lamaziCart3OfCart . drop1s . (imat <*>) . add1s) d

tlamazi4 [rotx, roty] mat imat d = mapcols (dofr3 . sphOfCart3 . (rotY (rofd -roty) <*>) . (rotX (rofd -rotx) <*>) . lamaziCart3OfCart . drop1s . add1s . lamaziCartOfCart3 . (rotX (rofd rotx) <*>) . (rotY (rofd roty) <*>) . cart3OfSph . rofd3 1) d

lamaziParams (v0, ll0, v1, ll1) =
   let
      v0a = lamaziOfSph (1, vec 0 0, 0) ll0
      v1a = lamaziOfSph (1, vec 0 0, 0) ll1
      d10 = v1 <-> v0
      d10a = v1a <-> v0a
      scale = modulus d10 / modulus d10a
      longsh = argument d10 - argument d10a
      -- longsh = 2.5
      v0b = lamaziOfSph (scale, vec 0 0, longsh) ll0
      sh = v0 <-> v0b
    in
      (scale, sh, longsh)

initl = [[1, 0, 0, 0, 0, 0, 0]] :: [[Double]]
init1 = [1, 1, 0, 0, 0] :: [Double]
init2 = [[0, 0]] :: [[Double]]
uo xs =
   let
      u = mean xs
      o = sqrt (mean (map (\x -> (x - u) ^ 2) xs)) / u
    in
      o

vmean vs = recip (fromIntegral $ length vs) |*| [map sum $ transpose vs]
mapcols f = map (only . f . sing)

getrsrs f =
   let
      vs2 = f init1 lls
      ce1 = vmean vs1
      ce2 = vmean vs2
      vs1a = mapcols (<-> ce1) vs1
      vs2a = mapcols (<-> ce2) vs2
      shift = ce1 <-> ce2
      rots = zipWith (\v1 v2 -> argument [v1] - argument [v2]) vs1a vs2a
      rot = mean rots
      vs2b = mapcols (rotation rot <*>) vs2a
      rotsb = zipWith (\v1 v2 -> argument [v1] - argument [v2]) vs1a vs2b
      rotb = mean rotsb
      scales = zipWith2d (/) vs1a vs2b
      scale = vmean scales
      vs2c = mapcols (zipWith2d (*) scale) vs2b
      args = hjk (scale, rot, shift)
      -- residuals = zipWith (<->) vs1a vs2c
      residuals = (vs1 <->) $ f args lls
    in
      (residuals, (scale, rot, shift))

getrsrsD f = let
   vs2 = f init1 lls
   ce1 = vmean vs1
   ce2 = vmean vs2
   vs1a = mapcols (<-> ce1) vs1
   vs2a = mapcols (<-> ce2) vs2
   shift = ce1 <-> ce2
   rots = zipWith (\v1 v2 -> argument [v1] - argument [v2]) vs1a vs2a
   rot = mean rots
   vs2b = mapcols (rotation rot <*>) vs2a
   rotsb = zipWith (\v1 v2 -> argument [v1] - argument [v2]) vs1a vs2b
   rotb = mean rotsb
   scales = zipWith2d (/) vs1a vs2b
   scale = vmean scales
   vs2c = mapcols (zipWith2d (*) scale) vs2b
   vs2d = mapcols (f (hjk (scale, rot, shift))) lls
   -- residuals = zipWith (<->) vs1a vs2c
   residuals = (vs1 <->) $ f (hjk (scale, rot, shift)) lls
   in
   --      (lls, vs1, vs2, ce1, ce2, vs1a, vs2a, shift, rots, rot, vs2b, scales, scale, vs2c, residuals)

   [["lls", "vs1", "vs2", "ce1", "ce2", "vs1a", "vs2a", "shift", "rots", "rot", "vs2b", "rotsb", "rotb", "scales", "scale", "vs2c", "vs2d", "residuals", "scale", "rot", "shift"], [show lls, show vs1, show vs2, show ce1, show ce2, show vs1a, show vs2a, show shift, show rots, show rot, show vs2b, show rotsb, show rotb, show scales, show scale, show vs2c, show vs2d, show residuals, show scale, show rot, show shift]]

getsrs rotLL = snd $ getrsrs rotLL

getres rotLL = fst $ getrsrs rotLL

getres1 f = sum $ map (^ 2) $ concat $ getres f

{-
xo0 = a * xi0 + b * yi0 + p
xo1 = a * xi1 + b * yi1 + p
xo2 = a * xi2 + b * yi2 + p

[ xi0 yi0 1 ] [ a ]
[ xi1 yi1 1 ] [ b ]
[ xi2 yi2 1 ] [ p ]

[ a b p ] [ xi0 xi1 xi2 ] = [ xo0 xo1 xo2 ]
[ c d q ] [ yi0 yi1 yi2 ]   [ yo0 yo1 yo2 ]
          [   1   1   1 ]
      M    *     I        = O
      M                   = O * I^-1

yo0 = c * xi0 + d * yi0 + q
yo1 = c * xi1 + d * yi1 + q
yo2 = c * xi2 + d * yi2 + q

[ a b c d e f g h i w ] [ xi0        xi1            xi9       ] = [ xo0 xo1 xo2 xo3 xo4 xo5 xo6 xo7 xo8 xo9 ]
[ j k l m n o p q r z ] [ yi0        yi1            yi9       ]   [ yo0 yo1 yo2 yo3 yo4 yo5 yo6 yo7 yo8 yo9 ]
                        [ xi0^2      xi1^2          xi9^2     ]
                        [ yi0^2      yi1^2          yi9^2     ]
                        [ xi0  *yi0  xi1  *yi1  ... xi9  *yi9 ]
                        [ xi0^3      xi1^3          xi9^3     ]
                        [ yi0^3      yi1^3          yi9^3     ]
                        [ xi0^2*yi0  xi1^2*yi1      xi9^2*yi9 ]
                        [ xi0*yi0^2  xi1*yi1^2      xi9*yi9^2 ]
                        [   1          1              1       ]
 -}

add1s = map (++ [1])

add1sa xs = map (\[x, y] -> [x, y, x^2, y^2, x*y, x^3, y^3, x^2*y, x*y^2, 1]) xs

drop1s = sing . take 2 . only

t3 = select [0, 2, 3]

id3 = [[1, 0], [0, 1], [0, 0]] --, [0, 0], [0, 0], [0, 0]]

nan = 0 / 0

isNaNM m = any (any isNaN) m

hjk ([[sx, sy]], r, [[xo, yo]]) = [sx, sy, r, xo, yo]

lamaziSRS = hjk (getsrs (lamazi3 centreLL))
centreLL = search getRes [0, 0] [11, 22] 8 100
--centreLL = [-37, -95]
lamaziMat = getLamaziMat centreLL
getLamaziMat rotLL = getMat (lamazi4 rotLL id3 lls) vs1
getLamaziMatD rotLL = getMatD (lamazi4 rotLL id3 lls) vs1

getMat i o = if isNaNM i then map2 (const nan) (t3 o) else transpose $ mvRegress (transpose $ add1s i) $ transpose o

--getMat i o = if isNaNM i then map2 (const nan) (t3 o) else t3 o <*> invMat (add1s $ t3 i)

getMatD i o = (t3 o <*>) <$> invMatD (add1s $ t3 i)

getRes rotLL = residuals $ vs1 <-> lamazi4 rotLL (getLamaziMat rotLL) lls

getResD rotLL = do
   m <- getLamaziMatD rotLL
   return $ residuals $ vs1 <-> lamazi4 rotLL m lls

residuals x = sqrt $ mean $ map (^ 2) $ concat x

-- centreLL = search (\rotLL -> getres1 (lamazi3 rotLL)) [0, 0] [5, 10] 16 30

-- search :: (Fractional a, Enum a, Ord a) => ([[a]] -> a) -> [[a]] -> [[a]] -> a -> Int -> [[a]]
-- search :: (Fractional a, Enum a, Ord a) => ([[a]] -> a) -> [[a]] -> [[a]] -> a -> Int -> [[a]]
search _ c _ _ 0 = c
search f c r n i =
   let
      xs1 = zipWith (\c1 r1 -> [c1 - r1 * n, c1 - r1 * (n - 1) .. c1 + r1 * n]) c r
      xs = crossList xs1
      fxxs = mapfxx f xs
      min = minimum fxxs
    in
      search f (snd min) (map (/ 1.2) r) n (i - 1)

sh = searchD getRes [0, 0] [12, 24] [8, 12]
shd = searchDD getResD [0, 0] [12, 24] 8

-- sh = searchD (\rotLL -> getres1 (\init1 ll -> ll)) [0, 0] [12, 24] 8

mapfxxM f xs = flip zip xs <$> mapM f xs

searchD f c r n =
   let
      xs1 = zipWith3 (\c1 r1 n1 -> [c1 - r1 * n1, c1 - r1 * (n1 - 1) .. c1 + r1 * n1]) c r n
      xs = crossList xs1
      fxxs = mapfxx f xs
      --min = minimum fxxs
    in
      fromAssocsD $ map (\(fx, x) -> (map toDyn x, fx)) fxxs

searchDD f c r n =
   do
      let xs1 = zipWith (\c1 r1 -> [c1 - r1 * n, c1 - r1 * (n - 1) .. c1 + r1 * n]) c r
      let xs = crossList xs1
      fxxs <- mapfxxM f xs
      let min = minimum fxxs
      return $ fromAssocsDA (+) 0 $ map (\(fx, x) -> (map toDyn x, fx)) fxxs

altx = sum $ map (4 ^) [0 .. 31]
alty = altx * 2
interleave [x, y] = sum $ map
   (\b -> let
      c = 2 ^ b
      d = 4 ^ b
      in (if x .&. c /= 0 then d * 2 else 0) + (if y .&. c /= 0 then d else 0)
   )
   [30, 29 .. 0]

inRect [[x0, y0], [x1, y1]] [[x, y]] = inRange x0 x1 x && inRange y0 y1 y

inRange a b x = x >= a && x <= b
between a b m = M.takeWhileAntitone (<= b) $ M.dropWhileAntitone (< a) m

getRect args@[v0, v1] m = M.filter (inRect args) $ between (interleave v0) (interleave v1) m

data Mode   = Waiting
            | MapScroll [[Double]] [[Double]]
            | PointMove [Double]

main2 = do
   initGUI

   mainWindow <- windowNew
   map1 <- drawingAreaNew

   pixbuf <- pixbufNewFromFile (atlas ++ "6262/060.png")

   vb  <- vBoxNew False 0
   vb1 <- vBoxNew False 0
   hb  <- hBoxNew False 0
   latw        <- hScaleNewWithRange -180 180 0.00000001
   longw       <- hScaleNewWithRange -180 180 0.00000001
   zoom        <- hScaleNewWithRange -6 6 0.1
   zoom1       <- hScaleNewWithRange -6 6 0.1
   fineAdjXW   <- hScaleNewWithRange -10 10 0.00000001
   fineAdjYW   <- hScaleNewWithRange -10 10 0.00000001

   fitXsW <- buttonNewWithLabel "Fit red lines to black crosses"
   zoomed <- drawingAreaNew
   boxPackStart vb1 latw      PackNatural 0
   boxPackStart vb1 longw     PackNatural 0
   boxPackStart vb1 zoom      PackNatural 0
   boxPackStart vb1 zoom1     PackNatural 0
   boxPackStart vb1 fineAdjXW PackNatural 0
   boxPackStart vb1 fineAdjYW PackNatural 0
   boxPackStart vb1 fitXsW    PackNatural 0
   boxPackStart vb  vb1       PackNatural 0
   boxPackStart vb  zoomed    PackGrow 0
   boxPackStart hb  vb        PackGrow 0
   boxPackStart hb  map1      PackGrow 0

   set
      mainWindow
      [ windowTitle := "Atlas"
      , windowDefaultWidth  := 1000
      , windowDefaultHeight := 1000
      ]
   zoomedPos  <- newIORef [[0, 0]]
   zoomedSize <- newIORef $ Rectangle 0 0 0 0
   mapScr     <- newIORef [[0, 1000]]
   guiMode    <- newIORef Waiting
   lvsref     <- mapM newIORef lvs
   movePoint  <- newIORef $ head lvsref
   centreLL   <- newIORef $ search getRes [0, 0] [11, 22] 8 100

   [lat, long] <- readIORef centreLL
   set latw  [rangeValue := lat]
   set longw [rangeValue := until (<180) (- 360) long]
   set zoom  [rangeValue := -2]

   let
      mapButtonPress = do
         zoom1 <- liftIO $ get zoom rangeValue
         let zoom2 = 2**zoom1
         (x2, y2) <- eventCoordinates
         let p2 = [[x2, y2]]
         mapScr1 <- liftIO $ readIORef mapScr
         let p = (1 / zoom2) |*| p2 <+> mapScr1
         lat  <- liftIO $ rangeGetValue latw
         long <- liftIO $ rangeGetValue longw

         let ll = [ lat, long ]
         let t = lamazi4a ll $ getLamaziMat ll
         lvs <- liftIO $ mapM readIORef lvsref
         let
            dists = mapfxx
               ( \llxy -> let
                     p1 = [drop 2 llxy]
                     in modulus (p <-> p1)
               )
               lvs
         let mindist = minimum dists
         liftIO $ do
            print ("x2 y2  = ", p2)
            print ("x y    = ", p)
            print ("scroll = ", mapScr1)
            print dists
            print mindist
            print (elemIndex mindist dists)
         if fst mindist < 50
            then liftIO $ do
               writeIORef movePoint $ lvsref !! fromJust (elemIndex mindist dists)
               writeIORef guiMode $ PointMove (snd mindist)
            else liftIO $ do
               writeIORef guiMode $ MapScroll p mapScr1
         return False

      mapButtonRelease = do
         liftIO $ writeIORef guiMode Waiting
         return False

      mapMotion = do
         g <- liftIO $ readIORef guiMode
         case g of
            Waiting       -> zoomedMove
            MapScroll _ _ -> mapScroll
            PointMove _   -> pointMove

      pointMove = do
         zoom1 <- liftIO $ get zoom rangeValue
         let zoom2 = 2**zoom1
         (x2, y2) <- eventCoordinates
         let p2 = [[x2, y2]]
         mapScr1 <- liftIO $ readIORef mapScr
         --MapScroll p1 mapScr1 <- liftIO $ readIORef guiMode
         ptref <- liftIO $ readIORef movePoint
         (la:lo:_) <- liftIO $ readIORef ptref
         let [[x, y]] = (1 / zoom2) |*| p2 <+> mapScr1
         liftIO $ writeIORef ptref [la, lo, x, y]
         liftIO $ writeIORef zoomedPos [[x, y]]
         liftIO $ widgetQueueDraw zoomed
         liftIO $ widgetQueueDraw map1

         --PointMove p3 <- liftIO $ readIORef guiMode
         return False

      zoomedMove = do
         zoom1 <- liftIO $ get zoom rangeValue
         let zoom2 = 2**zoom1
         (x2, y2) <- eventCoordinates
         let p2 = [[x2, y2]]
         mapScr1 <- liftIO $ readIORef mapScr
         let p = (1 / zoom2) |*| p2 <+> mapScr1
         liftIO $ writeIORef zoomedPos p
         liftIO $ widgetQueueDraw zoomed
         return False
{-
      pointMove = do
         lat  <- liftIO $ rangeGetValue latw
         long <- liftIO $ rangeGetValue longw

         let ll = [ lat, long ]
         let t = lamazi4a ll $ getLamaziMat ll
         return False
-}
      mapScroll = do
         zoom1 <- liftIO $ get zoom rangeValue
         let zoom2 = 2**zoom1
         (x2, y2) <- eventCoordinates
         let p2 = [[x2, y2]]
         MapScroll p1 mapScr1 <- liftIO $ readIORef guiMode
         let p = (1 / zoom2) |*| p2 <+> mapScr1
         liftIO $ do
            print "--------------------"
            print ("p1    =", p1)
            print ("scroll=", mapScr1)
            print ("x2 y2 =", p2)
            print ("x y   =", p)
         liftIO $ writeIORef mapScr (p1 <-> p <+> mapScr1)
         liftIO $ widgetQueueDraw map1
         return False

      mapWheel = do
         zoom1 <- liftIO $ get zoom rangeValue
         let zoom2 = 2**zoom1
         (x2, y2) <- eventCoordinates
         let p2 = [[x2, y2]]
         mapScr1 <- liftIO $ readIORef mapScr
         let p = (1 / zoom2) |*| p2 <+> mapScr1
         d <- eventScrollDirection
         let
            zoom3 = case d of
               ScrollUp   -> zoom1 + 0.25
               ScrollDown -> zoom1 - 0.25
         let zoom4 = 2**zoom3
         liftIO $ writeIORef mapScr (p <-> (1 / zoom4) |*| p2)
         liftIO $ set zoom [ rangeValue := zoom3 ]
         liftIO $ widgetQueueDraw map1
         return False

      --eventWindowSize :: Num b => Render  (b, b)
      eventWindowSize map1 = do
         --map1 <- eventWindow
         w <- liftIO $ drawWindowGetWidth map1
         h <- liftIO $ drawWindowGetHeight map1
         return $ if w*h > 1
            then (fromIntegral w, fromIntegral h)
            else (1, 1)

      barMove a b = do
         lat  <- liftIO $ get latw  rangeValue
         long <- liftIO $ get longw rangeValue
         liftIO $ writeIORef centreLL [lat, long]
         mapsDraw
         return False

      mapsDraw = do
         widgetQueueDraw map1
         widgetQueueDraw zoomed
         return False

      zoomedDraw = do
         xs <- liftIO $ widgetGetAllocatedWidth  zoomed
         ys <- liftIO $ widgetGetAllocatedHeight zoomed
         p <- liftIO $ readIORef zoomedPos
         let [[x, y]] = p
         translate (fromIntegral xs / 2) (fromIntegral ys / 2)
         scale 4 4
         translate -x -y
         mapDraw1

      mapDraw = do
         zoom1 <- liftIO $ get zoom rangeValue
         let zoom2 = 2**zoom1
         p <- liftIO $ readIORef mapScr
         let [[x, y]] = p
         scale zoom2 zoom2
         translate -x -y
         mapDraw1

      mapDraw1 = do
         centreLL1 <- liftIO $ readIORef centreLL
         setSourcePixbuf pixbuf 0 0
         Cairo.paint
         setSourceRGB 1 0 0
         setLineWidth 2

         let m  = getLamaziMat centreLL1
         let im = invMat (take 2 m) <*> (identity (2, 2) <++> (-1 |*| drop 2 m))
         let t  = lamazi4 centreLL1 m
         let it = ilamazi4 centreLL1 im
         let tt = tlamazi4 centreLL1 m im
         liftIO $ putStrLn ""
         liftIO $ print $ GridV m
         liftIO $ print $ GridV im
         liftIO $ print $ GridV (im <*> m)
         liftIO $ print $ searchD (only . (im <*>) . add1s . (m <*>) . add1s . sing) [0, 0] [22, 22] [4, 8]
         let (lat0, lat1, long0, long1) = sphereGridBounds it 0 6262 626 0 4312 431
         liftIO $ print (lat0, lat1, long0, long1)
         sphereGridPatch3 t lat0 lat1 long0 long1 5 5
         --sphereGrid t 5 5
         setSourceRGB 0 0 0
         lvs <- liftIO $ mapM readIORef lvsref
         mapM_
            ( \(la:lo:x:y:_) -> do
                  newPath
                  movTo [[x-50, y-50]]
                  linTo [[x+50, y+50]]
                  movTo [[x-50, y+50]]
                  linTo [[x+50, y-50]]
                  stroke
            )
            lvs

      fitXs = do
         let [lat, long] = search getRes [0, 0] [11, 22] 8 100
         liftIO $ writeIORef centreLL [lat, long]
         liftIO $ set latw  [rangeValue := lat]
         liftIO $ set longw [rangeValue := until (<180) (- 360) long]
         liftIO $ widgetQueueDraw map1
         liftIO $ widgetQueueDraw zoomed
         return False
{-
[a b c] [x] = ax+by+c
[d e f] [y]   dx+ey+f
        [1]

[1 0 -c] [ax+by+c]   [ax+by]
[0 1 -f] [dx+ey+f] = [dx+ey]
         [1]         

[a b]^-1 [ax+by] = [x]
[d e]    [dx+ey]   [y]

[a b]^-1 [1 0 -c] [ax+by+c]   [x]
[d e]    [0 1 -f] [dx+ey+f] = [y]


g(ax+by+c) + h(dx+ey+f) = x
i(ax+by+c) + j(dx+ey+f) = y

-}

      fitGrid :: EventM EButton Bool = do
         lat  <- liftIO $ rangeGetValue latw
         long <- liftIO $ rangeGetValue longw
         let centre = [ lat, long ]
         let m = getLamaziMat centre
         let t = lamazi4 centre m
         let it = ilamazi4 centre $ invMat m
         let (lat0, lat1, long0, long1) = sphereGridBounds it 0 6262 626 0 4312 431
         let lines = sphereGridPatch2 t lat0 lat1 long0 long1 5 5
         return False

   widgetAddEvents map1 [PointerMotionMask]
   on map1        draw                 mapDraw
   on map1        buttonPressEvent     mapButtonPress
   on map1        buttonReleaseEvent   mapButtonRelease
   on map1        motionNotifyEvent    mapMotion
   on map1        scrollEvent          mapWheel
   on zoomed      draw                 zoomedDraw
   on mainWindow  objectDestroy        mainQuit
   on latw        changeValue          barMove
   on longw       changeValue          barMove
   on zoom        changeValue          barMove
   on fitXsW      buttonPressEvent     fitXs
   containerAdd  mainWindow hb
   widgetShowAll mainWindow

   --    resume

   mainGUI

data DirPure = DirPure String [PagePure] deriving (Eq, Ord, Show, Read)
data Dir = Dir (IORef String) (IORef [IORef Page]) deriving (Eq)

data PagePure = PagePure String [[Double]] deriving (Eq, Ord, Show, Read)
data Page = Page (IORef String) (IORef [IORef [Double]])

load file = readFromFile file >>= pureToRef
save file dir = refToPure dir >>= showToFile file

readFromFile file = read <$> readFile file
showToFile file a = writeFile file $ show a

pureToRef (DirPure p ps) = do
   pr <- newIORef p
   prs <- mapM (\(PagePure f xs) -> do
      fr <- newIORef f
      xsr <- mapM newIORef xs
      xsrr <- newIORef xsr
      newIORef $ Page fr xsrr) ps
   prsr <- newIORef prs
   return $ Dir pr prsr

refToPure (Dir pr prsr) = do
   p <- readIORef pr
   prs <- readIORef prsr
   ps <- mapM (\pr -> do
      Page fr xsrr <- readIORef pr
      f <- readIORef fr
      xsr <- readIORef xsrr
      xs <- mapM readIORef xsr
      return $ PagePure f xs) prs
   return $ DirPure p ps

sphereGridBounds it x0 x1 xstep y0 y1 ystep = let
   xs     = [x0, x0 + xstep .. x1 ]
   ys     = [y0, y0 + ystep .. y1 ]
   vs     = map (\y -> [x0, y]) ys
         ++ map (\y -> [x1, y]) ys
         ++ map (\x -> [x, y0]) xs
         ++ map (\x -> [x, y1]) xs
   [lats, longs] = transpose $ it vs
   ls     = sort longs
   ls1    = zip ls (tail ls ++ [head ls])
   ls2    = snd $ maximum $ mapfxx (until (<180) (- 360) . uncurry (-)) ls1

   in (minimum lats, maximum lats, snd ls2, fst ls2)

frac x = x - fromIntegral (floor x)

mod360 x = modRF x 360

modRF x y = frac (x / y) * y

modRF2 x y m = let f = frac (x / y) in (if f > m then f - 1 else f) * y

--buttonDA :: EventM EButton Bool
sphereGrid t = sphereGridPatch t -90 90 -180 180

sphereGridPatch3 t lat0 lat1 long0 long1 maj min = sphereGridPatch t (floorBy lat0 maj) (ceilingBy lat1 maj) (floorBy long0 maj) (ceilingBy long1 maj) maj min

sphereGridPatch t lat0 lat1 long0 long1 maj min = do
   mapM_ (\long -> do
      newPath
      movTo $ t [[lat0, long]]
      mapM_ (\lat -> do
         linTo $ t [[lat, long]]
         ) [lat0 + min, lat0 + min*2 .. lat1]
      stroke
      ) [long0, long0 + maj .. long1]
   mapM_ (\lat -> do
      newPath
      movTo $ t [[lat, long0]]
      mapM_ (\long -> do
         linTo $ t [[lat, long]]
         ) [long0 + min, long0 + min*2 .. long1]
      stroke
      ) [lat0, lat0 + maj .. lat1]

floorBy x y = fromIntegral (floor (x / y)) * y
ceilingBy x y = fromIntegral (ceiling (x / y)) * y

sphereGridPatch2 t lat0 lat1 long0 long1 maj min = let
   latsmaj  = [ lat0,  lat0 + maj ..  lat1]
   latsmin  = [ lat0,  lat0 + min ..  lat1]
   longsmaj = [long0, long0 + maj .. long1]
   longsmin = [long0, long0 + min .. long1]
   in concatMap (\ lat -> zipTail (\long0 long1 -> t [[lat , long0]] ++ t [[lat , long1]]) longsmin)  latsmaj
   ++ concatMap (\long -> zipTail (\ lat0  lat1 -> t [[lat0, long ]] ++ t [[lat1, long ]])  latsmin) longsmaj


zipTail f xs = zipWith f xs $ tail xs

lineData a b = let
   d = b <-> a
   l = modulus d
   u = (1 / l) |*| d
   [[x, y]] = u
   i = invMat [[x, y], [-y, x]]
   in (l, a, i)

lineDist (l, a, i) r = let
   ra = r <-> a
   in i <*> ra
   --[[x, y]] = i <*> r
   --in x > -t && x < l + t && abs y < t

gmv = do
   setCurrentDirectory "/home/brett/Documents/Info/earth"
   -- sequence $ concat $ crossWith (\y x -> callCommand ("mv earth6_" ++ fmt1 3 (-y) ++ "_" ++ fmt1 3 x ++ ".png earth6_" ++ fmt 2 y ++ "_" ++ fmt 3 (x - if x > 180 then 360 else 0) ++ ".png")) [-75, -50 .. 50] [90, 120 .. 330]
   --   sequence $ concat $ crossWith (\y x -> callCommand ("mv earth6_" ++ fmt 2 y ++ "_" ++ fmt 3 (x - if x > 180 then 360 else 0) ++ ".png earth6_" ++ fmt 2 y ++ "_" ++ fmt1 3 x ++ ".png")) [-75, -50 .. 50] [30, 60]
   sequence $ concat $ crossWith (\y x -> callCommand ("mv earth6_" ++ fmt 2 y ++ "_" ++ fmt1 3 x ++ ".png earth6_" ++ fmt1 3 (y + 100) ++ "_" ++ fmt1 3 x ++ ".png")) [-75, -50 .. 50] [0, 30 .. 330]

main1 =
   mapM_
      ( \n -> do
            setCurrentDirectory "/home/brett/Documents/Info/Atlas/5794"
            Right im1 <- readImage (fmt1 3 n ++ ".png")
            print $ itype im1
            let ImageRGB8 im = im1
            let page = generateImage (fgAux im) 5794 3785
            savePngImage ("grid/" ++ fmt1 3 n ++ ".png") (ImageRGB8 page)
      )
      [60]

hough = do
   let size = 16
   let count = size^2
   let angleRange = 15
   let distRange = 15
   crossWithM_ (\angle dist -> withImageSurface FormatRGB24 1920 1080 $ \surface -> do
            renderWith surface $ do
               setLineWidth 1
               setSourceRGB 1 1 1
               moveTo (cos angle) (sin angle)
            --surfaceWriteToPNG surface ("x" ++ fi)
            ) [0..15] [0..15]

hough2 = do
   pixbuf <- pixbufNewFromFile "/home/brett/Documents/Info/Atlas/5794/060.png"
   x <- pixbufGetWidth pixbuf
   y <- pixbufGetHeight pixbuf
   let z = ceiling $ sqrt $ fromIntegral (x^2 + y^2)
   let z2 = fromIntegral z / 2
   mapM_ (\angle -> withImageSurface FormatRGB24 z z $ \surface -> do
      renderWith surface $ do
         Cairo.translate z2 z2
         Cairo.rotate $ rofd angle
         setSourcePixbuf pixbuf 0 0
         Cairo.paint) [0..359]

flatten src x y = do
   withImageSurface FormatRGB24 1 y $ \dest -> do
      renderWith dest $ do
         mapM_ (\x1 -> do
            setSourceSurface src (fromIntegral x1) 0
            setOperator OperatorAdd
            Cairo.paint)
            [0..x-1]
      return dest

flatten2 src x y = do
   let xh = div x 2
   withImageSurface FormatRGB24 xh y $ \dest -> do
      renderWith dest $ do
         setSourceSurface src 0 0
         setOperator OperatorAdd
         Cairo.paint
         setSourceSurface src -(fromIntegral xh) 0
         setOperator OperatorAdd
         Cairo.paint

fgAux im x y =
   let
      PixelRGB8 r g b = pixelAt im x y
      r1 = fromIntegral r
      g1 = fromIntegral g
      b1 = fromIntegral b
      t = r1+g1+b1+1
      rp = r1*50/t
      gp = g1*50/t
      bp = b1*50/t
      z = max 0 $ 255 - (5 * abs (rp - 10) * abs (gp - 20)^2 * abs (bp - 20)^2 * sqrt (abs (t - 500)))
      z1 = round z
    in
      PixelRGB8 z1 z1 z1

lvs =
   [ [25, -120,  419, 3603]
   , [25, -95,  3305, 3928]
   , [25, -75,  5619, 3679]
   , [50, -125,  839,  346]
   , [50, -95,  3271,  734]
   , [50, -70,  5309,  419]
   , [35, -120,  704, 2324]
   , [35, -95,  3292, 2666]
   , [35, -70,  5872, 2266]
   , [40, -115, 1335, 1822]
   ]
--russia 192
--75N 15E = 1579,64
--60N 0E  = 181,186
--30N 50E =  45,3393
--35N 145E = 5496,3559
lvsr =
   [ [60,  0,  181,  186]
   , [30, 50,   45, 3393]
   , [35,145, 5496, 3359]
   ]

lls = take 10 $ map (take 2) lvs

vs1 = take 10 $ map (drop 2) lvs

{-
419, 3603 = 120W 25N
3305, 3928 = 95W 25N
5619, 3679 = 75W 25N

839, 346 = 125W 50N
3271, 734 = 95W 50N
5309, 419 = 70W 50N
-}
-- gridColor = 137 198 219
arcCentre x0 y0 x1 y1 x2 y2 =
   let
      xa = (x0 + x1) / 2
      ya = (y0 + y1) / 2
      xb = (x1 + x2) / 2
      yb = (y1 + y2) / 2
      xc = x1 - x0
      yc = y1 - y0
      xd = x2 - x1
      yd = y2 - y1
      e = xa * xc + ya * yc
      f = xb * xd + yb * yd
      {-
      j * xc - k * xd = xb - xa
      j * yc - k * yd = yb - ya
      -}
      mat = [[xc, yc, e], [xd, yd, f]]
      -- in mat

      [[_, _, x], [_, _, y]] = gaussElim [[xc, yc, e], [xd, yd, f]]
    in
      (x, y, sqrt ((x - x1) ^ 2 + (y - y1) ^ 2))

t = arcCentre 419 3603 3305 3928 5619 3679

t1 = lamaziParams (vec 419 3603, rofd2 $ vec 25 -120, vec 5619 3679, rofd2 $ vec 25 -75)

sphereGridPatch1 t lat0 lat1 long0 long1 maj min = do
   let longs = [long0, long0 + min .. long1]
   let lats  = [lat0 , lat0  + min .. lat1 ]
   let min1 = min / 3
   let min2 = min1 * 2
   mapM_ (\long -> do
      newPath
      movTo (t lat0 long)
      mapM_ (\lat4 -> do
         let lat2 = lat4 - min1
         let lat3 = lat4 - min2
         let [[x2, y2]] = t lat2 long
         let [[x3, y3]] = t lat3 long
         let [[x4, y4]] = t lat4 long
         curveTo x2 y2 x3 y3 x4 y4
         ) $ tail lats
      stroke
      ) [long0, long0 + maj .. long1]
   mapM_ (\lat -> do
      newPath
      movTo (t lat long0)
      mapM_ (\long4 -> do
         let long2 = long4 - min1
         let long3 = long4 - min2
         let [[x2, y2]] = t lat long2
         let [[x3, y3]] = t lat long3
         let [[x4, y4]] = t lat long4
         curveTo x2 y2 x3 y3 x4 y4
         ) $ tail longs
      stroke
      ) [lat0, lat0 + maj .. lat1]

