-- Copyright 2025 Brett Curtis
{-# LANGUAGE LexicalNegation #-}

module Atlas where

import Favs hiding (on)
import Https
import MHashDynamic
import Matrix hiding ((*), (+), (-))
import MyPretty2

import Prelude hiding ((<*>))

import Data.Bits
import Data.Map qualified as M

import Codec.Picture
import Data.List
import System.Directory
import System.Process

import Graphics.UI.Gtk

-- import Graphics.UI.Gtk.Abstract.Widget
-- import Graphics.UI.Gtk.Misc.DrawingArea
import Graphics.Rendering.Cairo
import Graphics.Rendering.Cairo qualified as Cairo
import Graphics.UI.Gtk.Gdk.EventM

-- import Graphics.UI.Gtk.General.Structs

fmt n x = let s = show $ abs x in (if x < 0 then "-" else "+") ++ drop (length s) (replicate n '0') ++ s
fmt1 n x = let s = show $ abs x in (if x < 0 then "-" else "") ++ drop (length s) (replicate n '0') ++ s

{-
makepages = mapM_ (\n -> writeFile ("page"++fmt n++".html") $
   formatHtml $ html [] $ gridH [
      "<html>",
      "   <body>",
      "      <table>",
      "         <tr><td><img src='pic"++fmt n++".png'></td><td><img src='pic"++fmt (n+1)++".png'></td></tr>",
      "      </table>",
      "   </body>",
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
  , --   "South America City Plans",

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

index =
  writeFile (path ++ "index.html") $
    formatH 1 $
      html defstyle $
        s $
          gridH
            [ [with1 "width" "100%" $ gridH $ map2 button [["PREV", "NORTH", "NEXT"], ["WEST", "HOME", "EAST"], ["IN", "SOUTH", "OUT"]]]
            ,
              [ cellA [("height", "1700")] $ iframe "contents" "contents.html"
              , cellA [("width", "91%"), ("height", "1700")] $ iframe "main" "index1.html"
              ]
            ]

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
        Right (ImageRGB8 left) <- readImage ("pic" ++ fmt1 3 n ++ ".png")
        Right (ImageRGB8 right) <- readImage ("pic" ++ fmt1 3 (n + 1) ++ ".png")
        let page = generateImage (\x y -> if x < 3131 then pixelAt left x y else pixelAt right (x - 3131) y) 6262 4312
        savePngImage ("joined" ++ fmt1 3 n ++ ".png") (ImageRGB8 page)
    )
    [2, 4 .. 434]

small =
  mapM_
    ( \n -> do
        setCurrentDirectory "/home/brett/Documents/Info/Atlas"
        Right (ImageRGB8 im) <- readImage ("joined/" ++ fmt 3 n ++ ".png")
        let page = generateImage (\x y -> pixelAt im (x * 19) (y * 19)) 320 220
        savePngImage ("small/" ++ fmt1 3 n ++ ".png") (ImageRGB8 page)
    )
    [2, 4 .. 434]

itype (ImageY8 i) = "Y8"
itype (ImageY16 i) = "Y16"
itype (ImageY32 i) = "Y32"
itype (ImageYF i) = "YF"
itype (ImageYA8 i) = "YA8"
itype (ImageYA16 i) = "YA16"
itype (ImageRGB8 i) = "RGB8"
itype (ImageRGB16 i) = "RGB16"
itype (ImageRGBA8 i) = "RGBA8"
itype (ImageRGBA16 i) = "RGBA16"
itype (ImageRGBF i) = "RGBF"
itype (ImageYCbCr8 i) = "YCbCr8"
itype (ImageCMYK8 i) = "CMYK8"
itype (ImageCMYK16 i) = "CMYK16"

test = do
  setCurrentDirectory "/home/brett/Documents/Info/Atlas"
  Right i <- readImage "pic001.png"
  putStrLn $ itype i

main1 = do
  initGUI

  mainWindow <- windowNew
  drawingArea <- drawingAreaNew

  pixbuf <- pixbufNewFromFile "/home/brett/Documents/Info/Atlas/6262/060.png"
  track <- do
    let dr = drawingArea
    {-
    widgetAddEvents dr [PointerMotionMask]
    on dr motionNotifyEvent $ do
       (r,t) <- eventPolarCoordinates
       liftIO $ if (0.8<r && r<1.2)
          then setJam (Just t)
          else setJam Nothing
       liftIO $ widgetQueueDraw dr
       return True

    on dr leaveNotifyEvent $ liftIO $
       setJam Nothing >> return True
       -}
    on dr draw $ drawGrid1 pixbuf
    -- scale 0.01 0.01
    {-
    scale 0.3 0.3
    --w <- liftIO (fromIntegral <$> widgetGetAllocatedWidth dr)
    --liftIO (fromIntegral <$> widgetGetAllocatedHeight dr)
    -- jam <- liftIO getJam
    -- cars <- liftIO getCars
    -- translate (w/2) (h/2)
    -- scale (w/drawSide) (h/drawSide)
    drawGrid1 pixbuf
    -}
    -- return True

    -- af <- aspectFrameNew 0.5 0.5 (Just 1)
    -- frameSetShadowType af ShadowNone
    -- containerAdd af dr
    return dr

  -- 'layout' is a widget that contains all interface elements
  -- properly arranged.

  layout <- do
    vb <- vBoxNew False 0
    hb <- hBoxNew False 0
    xoff <- hScaleNewWithRange -3500 3500 10
    yoff <- hScaleNewWithRange -2500 2500 10
    boxPackStart vb xoff PackNatural 0
    boxPackStart vb yoff PackNatural 0
    boxPackStart hb vb PackNatural 0
    boxPackStart hb track PackGrow 0
    return hb

  set
    mainWindow
    [ windowTitle := "S.A.R.A.H."
    , windowDefaultWidth := 400
    , windowDefaultHeight := 400
    ]
  on mainWindow objectDestroy mainQuit
  containerAdd mainWindow layout
  widgetShowAll mainWindow

  --   resume

  mainGUI

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

sphOfCart3 [[x, y, z]] =
  let
    [[northLen, long]] = polOfCart [[x, y]]
    [[r, lat]] = polOfCart [[z, northLen]]
   in
    [[r, lat, long]]

{-
cart3OfSph [[r, lat, long]] =
  let
    [[z, greenLen]] = cartOfPol [[r, lat]]
    [[x, y]] = cartOfPol [[greenLen, long]]
   in
    [[x, y, z]]
-}
cart3OfSph [[r, lat, long]] = [[r * sin long, -(r * sin lat), r * cos long * cos lat]]

lamaziOfSph (scale, sh, longsh) v = scalesh (scale, sh) $ cartOfPol $ lamaziOfSph1 $ v <+> vec 0 longsh
sphOfLamazi (scale, sh, longsh) v = (<-> vec 0 longsh) $ sphOfLamazi1 $ polOfCart v

lamaziCart3OfCart [[x, y]] = [[sqrt (1 - r2 / 4) * x, sqrt (1 - r2 / 4) * y, 1 - r2 / 2]]
 where
  r2 = x ^ 2 + y ^ 2

lamaziCartOfCart3 q = case q of
  [[x, y, z]] -> [[sqrt (2 / (1 + z)) * x, sqrt (2 / (1 + z)) * y]]
  p -> error $ show p

uns x = case x of
  [y] -> y
  z -> error ("uns on " ++ show z)

lamazi3 :: [Double] -> [Double] -> [[Double]] -> [[Double]]
lamazi3 [rotx, rotz] [scalex, scaley, rot, xoff, yoff] d = mapcols (scalerotsh (vec scalex scaley, rot, vec xoff yoff) . lamaziCartOfCart3 . ((rotX (rofd (rotx)) <*> rotY (rofd rotz)) <*>) . cart3OfSph . rofd3 1) d

lamazi4 [rotx, roty] mat d = mapcols ((mat <*>) . add1s . lamaziCartOfCart3 . (rotX (rofd rotx) <*>) . (rotY (rofd roty) <*>) . cart3OfSph . rofd3 1) d

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
mapcols f = map (uns . f . singleton)

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

getrsrsD f =
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
    vs2d = mapcols (f (hjk (scale, rot, shift))) lls
    -- residuals = zipWith (<->) vs1a vs2c
    residuals = (vs1 <->) $ f (hjk (scale, rot, shift)) lls
   in
    --    (lls, vs1, vs2, ce1, ce2, vs1a, vs2a, shift, rots, rot, vs2b, scales, scale, vs2c, residuals)

    [["lls", "vs1", "vs2", "ce1", "ce2", "vs1a", "vs2a", "shift", "rots", "rot", "vs2b", "rotsb", "rotb", "scales", "scale", "vs2c", "vs2d", "residuals", "scale", "rot", "shift"], [show lls, show vs1, show vs2, show ce1, show ce2, show vs1a, show vs2a, show shift, show rots, show rot, show vs2b, show rotsb, show rotb, show scales, show scale, show vs2c, show vs2d, show residuals, show scale, show rot, show shift]]

getsrs rr = snd $ getrsrs rr

getres rr = fst $ getrsrs rr

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
    M    *      I        =       O
    M                    =       O * I^-1

yo0 = c * xi0 + d * yi0 + q
yo1 = c * xi1 + d * yi1 + q
yo2 = c * xi2 + d * yi2 + q
 -}

add1s = map (++ [1])

t3 = select [0, 2, 3]

nan = 0 / 0

isNaNM m = any (any isNaN) m

getMat i o = if isNaNM i then map2 (const nan) (t3 o) else t3 o <*> invMat (add1s $ t3 i)

getMatD i o = (t3 o <*>) <$> invMatD (add1s $ t3 i)

getresMat rr = residuals $ vs1 <-> lamazi4 rr (getlamaziMat rr) lls

getresMatD rr = do
  m <- lamaziMatD rr
  return $ residuals $ vs1 <-> lamazi4 rr m lls

residuals x = sqrt $ mean $ map (^ 2) $ concat x

-- lamaziRR = search (\rr -> getres1 (lamazi3 rr)) [0, 0] [5, 10] 16 30

hjk ([[sx, sy]], r, [[xo, yo]]) = [sx, sy, r, xo, yo]

lamaziSRS = hjk (getsrs (lamazi3 lamaziRR))
lamaziRR = search getresMat [0, 0] [5, 10] 16 50
lamaziMat = getlamaziMat lamaziRR
getlamaziMat rr = getMat (lamazi4 rr [[1, 0], [0, 1], [0, 0]] lls) vs1
lamaziMatD rr = getMatD (lamazi4 rr [[1, 0], [0, 1], [0, 0]] lls) vs1

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
    search f (snd min) (map (/ 2) r) n (i - 1)

sh = searchD getresMat [0, 0] [12, 24] 8
shd = searchDD getresMatD [0, 0] [12, 24] 8

-- sh = searchD (\rr -> getres1 (\init1 ll -> ll)) [0, 0] [12, 24] 8

mapfxxM f xs = flip zip xs <$> mapM f xs

searchD f c r n =
  let
    xs1 = zipWith (\c1 r1 -> [c1 - r1 * n, c1 - r1 * (n - 1) .. c1 + r1 * n]) c r
    xs = crossList xs1
    fxxs = mapfxx f xs
    min = minimum fxxs
   in
    fromAssocsDA (+) 0 $ map (\(fx, x) -> (map toDyn x, fx)) fxxs

searchDD f c r n =
  do
    let xs1 = zipWith (\c1 r1 -> [c1 - r1 * n, c1 - r1 * (n - 1) .. c1 + r1 * n]) c r
    let xs = crossList xs1
    fxxs <- mapfxxM f xs
    let min = minimum fxxs
    return $ fromAssocsDA (+) 0 $ map (\(fx, x) -> (map toDyn x, fx)) fxxs

altx = sum $ map (4 ^) [0 .. 31]
alty = altx * 2
interleave [x, y] =
  sum $
    map
      ( \b ->
          let c = 2 ^ b
              d = 4 ^ b
           in (if x .&. c /= 0 then d * 2 else 0) + (if y .&. c /= 0 then d else 0)
      )
      [30, 29 .. 0]

inRect [[x0, y0], [x1, y1]] [[x, y]] = inRange x0 x1 x && inRange y0 y1 y

inRange a b x = x >= a && x <= b
between a b m = M.takeWhileAntitone (<= b) $ M.dropWhileAntitone (< a) m

getRect args@[v0, v1] m = M.filter (inRect args) $ between (interleave v0) (interleave v1) m

drawGrid1 pixbuf = do
  scale 0.3 0.3
  setSourcePixbuf pixbuf 0 0
  Cairo.paint
  setSourceRGB 1 0 1
  setLineWidth 5

  -- let t = lamaziOfSph t1 . rofd2
  let t = lamazi4 lamaziRR lamaziMat
  newPath
  sequence_ $ concat $ crossWith (\x y -> do movTo (t [[x, y]]); linTo (t [[x + 1, y]])) [-85, -84 .. 85] [-180, -175 .. 180]
  sequence_ $ concat $ crossWith (\x y -> do movTo (t [[x, y]]); linTo (t [[x, y + 1]])) [-85, -80 .. 85] [-180, -179 .. 180]
  stroke
  setSourceRGB 0 1 0
  mapM_
    ( \[lat, long] -> do
        newPath
        let z = t $ vec lat long
        arc (xofv z) (yofv z) 100 0 (2 * pi)
        stroke
    )
    lls

gmv = do
  setCurrentDirectory "/home/brett/Documents/Info/earth"
  -- sequence $ concat $ crossWith (\y x -> callCommand ("mv earth6_" ++ fmt1 3 (-y) ++ "_" ++ fmt1 3 x ++ ".png earth6_" ++ fmt 2 y ++ "_" ++ fmt 3 (x - if x > 180 then 360 else 0) ++ ".png")) [-75, -50 .. 50] [90, 120 .. 330]
  --  sequence $ concat $ crossWith (\y x -> callCommand ("mv earth6_" ++ fmt 2 y ++ "_" ++ fmt 3 (x - if x > 180 then 360 else 0) ++ ".png earth6_" ++ fmt 2 y ++ "_" ++ fmt1 3 x ++ ".png")) [-75, -50 .. 50] [30, 60]
  sequence $ concat $ crossWith (\y x -> callCommand ("mv earth6_" ++ fmt 2 y ++ "_" ++ fmt1 3 x ++ ".png earth6_" ++ fmt1 3 (y + 100) ++ "_" ++ fmt1 3 x ++ ".png")) [-75, -50 .. 50] [0, 30 .. 330]

main =
  mapM_
    ( \n -> do
        setCurrentDirectory "/home/brett/Documents/Info/Atlas"
        Right im1 <- readImage ("5794/" ++ fmt1 3 n ++ ".png")
        print $ itype im1
        let ImageRGB8 im = im1
        let page = generateImage (fgAux im) 5794 3785
        savePngImage ("grid/" ++ fmt1 3 n ++ ".png") (ImageRGB8 page)
    )
    [60]

fgAux im x y =
  let
    PixelRGB8 r g b = pixelAt im x y
    r1 = fromIntegral r
    g1 = fromIntegral g
    b1 = fromIntegral b
    z = 240 - 2 * min 120 (abs (r1 - 100) + abs (g1 - 185) + abs (b1 - 185))
    z1 = fromIntegral z
   in
    PixelRGB8 z1 z1 z1

lvs =
  [ [25, -120, 419, 3603]
  , [25, -95, 3305, 3928]
  , [25, -75, 5619, 3679]
  , [50, -125, 839, 346]
  , [50, -95, 3271, 734]
  , [50, -70, 5309, 419]
  ]

lls = take 6 $ map (take 2) lvs

vs1 = take 6 $ map (drop 2) lvs

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
