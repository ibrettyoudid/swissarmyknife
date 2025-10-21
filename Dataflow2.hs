{-# LANGUAGE MultiWayIf #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Move filter" #-}
{-# HLINT ignore "Use list comprehension" #-}

module Dataflow where

import Favs hiding (on)
import HTML
import MHashDynamic
import Matrix2 hiding ((*), (+), (-))
import MyPretty2 hiding (Term)

import Prelude hiding ((<*>))

import Data.Bits
import Data.Map qualified as M

import Codec.Picture
import Codec.Picture.Types
import Data.List
import Data.IORef
import Control.Monad
import System.Directory
import System.Process

import Graphics.UI.Gtk hiding (Box)
import Graphics.Rendering.Cairo
import Graphics.Rendering.Cairo qualified as Cairo
import Atlas (refToPure)

data TermType = Input | Output | Internal deriving (Eq, Ord, Show, Read)

data Object = Term { tat :: [Double] , selected :: Bool, ttype :: TermType }
            | Wire {                   selected :: Bool,                 kids :: [IORef Object] }
            | Box  { at :: [[Double]], selected :: Bool, text :: String, kids :: [IORef Object] }

movTo [x, y] = moveTo x y
linTo [x, y] = lineTo x y

rect [[x0, y0], [x1, y1]] = do
   newPath
   movTo [x0, y0]
   linTo [x1, y0]
   linTo [x1, y1]
   linTo [x0, y1]
   linTo [x0, y0]

inRange x y z = x >= y && x <= z

inRect [x, y] [[x0, y0], [x1, y1]] = inRange x x0 x1 && inRange y y0 y1

only [x] = x
sing x = [x]

data GuiMode = Waiting | Move [Double] | MaybeMove [Double]

data Recurse obj = Recurse obj (obj -> Recurse obj) 

main = do
   initGUI

   mainWindow <- windowNew
   area <- drawingAreaNew
   adj <- adjustmentNew 1 0 20 1 2 2
   inputNum  <- spinButtonNew adj 1 0
   outputNum <- spinButtonNew adj 1 0
   hbox <- hBoxNew False 0
   vbox <- vBoxNew False 0
   boxPackStart hbox inputNum  PackNatural 0
   boxPackStart hbox outputNum PackNatural 0
   boxPackStart vbox hbox      PackNatural 0
   boxPackStart vbox area      PackGrow 0

   containerAdd mainWindow vbox

   boxRef <- newIORef $ Box [[0, 0], [1000, 1000]] False "" [Term [300, 500] False Internal] [] [Box [[800, 500], [900, 600]] False "" [] [] []]
   guiMode <- newIORef Waiting
   let
      areaDraw = do
         setSourceRGB 0 0 0
         setLineWidth 1
         drawBox boxRef

      drawTerm termR = do
         term <- liftIO $ readIORef termR
         if tselected term
            then setSourceRGB 1 0 0
            else setSourceRGB 0 0 0.5
         rect [tat term <-> [3, 3], tat term <+> [3, 3]]
         fill

      drawWire wireR = do
         wire <- liftIO $ readIORef wireR
         colourSel $ wselected wire
         let [wt0, wt1] = wterms wire
         newPath
         movTo $ tat wt0
         linTo $ tat wt1
         stroke
         --mapM_ drawTerm $ wterms w

      drawBox boxR = do
         box <- liftIO $ readIORef boxR
         if selected box
            then setSourceRGB 1 0 0
            else setSourceRGB 0.5 0.5 0.5
         rect $ at box
         stroke
         mapM_ drawTerm $ terms box
         mapM_ drawWire $ wires box
         mapM_ drawBox  $ boxes box

            {-
            newPath
            let a = at b
            movTo $ head a
            mapM_ linTo $ tail a
            linTo $ head a
            stroke
            -}

      areaButton = do
         g <- liftIO $ readIORef guiMode
         case g of
            Waiting -> do
               b <- eventButton
               (x2, y2) <- eventCoordinates
               let hitAt = [x2, y2]
               box <- liftIO $ readIORef boxRef
               case b of
                  LeftButton -> do
                     liftIO $ writeIORef boxRef $ hitBox hitAt box
                     liftIO $ widgetQueueDraw area
                     liftIO $ writeIORef guiMode $ MaybeMove hitAt
                  RightButton -> do
                     liftIO $ startWires box
                     liftIO $ widgetQueueDraw area
                     liftIO $ writeIORef guiMode $ MaybeMove hitAt
         return False

      areaButtonRelease :: EventM EButton Bool = do
         liftIO $ writeIORef guiMode Waiting
         return False

      startWires terms box = let
         terms1 = mapM (\r -> readIORef r >>= newIORef) terms
         in return False

      getSelected objectRef = filter selected <$> subObjects objectRef

      subObjectRefs objectRef = do
         object <- readIORef objectRef
         subs <- mapM subObjectRefs $ kids object
         return $ objectRef:concat subs

      subObjects objectRef = mapM readIORef $ subObjectRefs objectRef

      hitRef hitAt objectR = do
         object <- liftIO $ readIORef objectR
         hit hitAt object

      hit :: [Double] -> Object -> IO [(Double, Object)]
      hit hitAt box@(Box {}) = do
         h <- mapM (hitRef hitAt) $ kids box
         if null h
               then if hitAt `inRect` at box 
                        then return [(0, box)]
                        else return []
               else return $ concat h

      hit hitAt term@(Term {}) = let 
         dist = modulus (hitAt <-> tat term)
         in return $ if dist < 10 then [(dist, term)] else []

      hit hat wire@(Wire {}) = do
         [wt0, wt1] <- mapM readIORef $ kids wire
         let 
            wl = tat wt1 <-> tat wt0
            wll = modulus wl
            wlu@[wlx, wly] = unit wl
            wwu = [wly, -wlx]
            [wrl, wrw] = [wlu, wwu] <*> (hat <-> tat wt0)
         return $ if wrl >= 0 && wrl < wll && abs wrw < 5
               then if | wrl / wll < 0.25 -> [(abs wrw, wt0)]
                       | wrl / wll > 0.75 -> [(abs wrw, wt1)]
                       | True             -> [(abs wrw, wire)]
               else
                  []
{-
      selSpansBoxes box = let
         n = (if selected box then (+1) else id) $ length $ filter hasSelected $ boxes box
         in n >= 2 || any selSpansBoxes (boxes box)
-}
      areaMotion = do
         g <- liftIO $ readIORef guiMode
         case g of
            Waiting        -> return False
            MaybeMove from -> maybeMove from
            Move      from -> move      from

      move from = do
         (x, y) <- eventCoordinates
         let to = [x, y]
         let by = to <-> from
         liftIO $ moveSelected by boxRef
         liftIO $ writeIORef guiMode $ Move to
         liftIO $ widgetQueueDraw area
         return False

      maybeMove from = do
         (x, y) <- eventCoordinates
         let to = [x, y]
         let by = to <-> from
         if modulus by > 3 
            then do
               liftIO $ writeIORef guiMode $ Move from
               move from
            else return False

      moveSelected by = visitPre (moveSelected1 by False)

      moveSelected1 by all obj = 
         if selected obj 
            then Recurse (moveObject by obj) (moveSelected1 by True)
            else Recurse                obj  (moveSelected1 by all )

      moveObject by  box@(Box  {}) = box  {  at = map (by <+>) (at box) }
      moveObject by term@(Term {}) = term { tat =      by <+>  tat term }
      moveObject by wire@(Wire {}) = wire

      visitPre1 f ref = do
         obj <- readIORef ref
         new <- f obj
         writeIORef ref new
         mapM_ (visitPre1 f) $ kids new

      visitPre f ref = do
         obj <- readIORef ref
         let Recurse new g = f obj
         writeIORef ref new
         mapM_ (visitPre g) $ kids new
{-
      setInputNumSelection n box = let
         box1 = box { boxes = map (setInputNumSelection n) $ boxes box }
         in if selected box
            then setInputNum n box1
            else box1

      setInputNum n box = let
         [[x, y], _] = at box
         terms1 = map (\i -> Term [x, y + i * 10 + 10] False Input) [1..n]
         in box { terms = terms1 ++ filter ((/= Input) . ttype) (terms box) }
         
      setOutputNumSelection n box = let
         box1 = box { boxes = map (setOutputNumSelection n) $ boxes box }
         in if selected box
            then setOutputNum n box1
            else box1

      setOutputNum n box = let
         [[_, y], [x, _]] = at box
         terms1 = map (\i -> Term [x, y + i * 10 + 10] False Output) [1..n]
         in box { terms = terms1 ++ filter ((/= Output) . ttype) (terms box) }
-}         


   widgetAddEvents area [PointerMotionMask]

   on area draw areaDraw
   on area buttonPressEvent    areaButton
   on area buttonReleaseEvent  areaButtonRelease
   on area motionNotifyEvent   areaMotion

   onValueSpinned inputNum $ do
      n <- get inputNum spinButtonValue 
      box <- liftIO $ readIORef boxRef
      --liftIO $ writeIORef boxRef $ setInputNumSelection n box
      liftIO $ widgetQueueDraw area

   onValueSpinned outputNum $ do
      n <- get outputNum spinButtonValue 
      box <- liftIO $ readIORef boxRef
      --liftIO $ writeIORef boxRef $ setOutputNumSelection n box
      liftIO $ widgetQueueDraw area
{-
   on inputNum  ValueSpinned    $ \a n -> do
      box <- liftIO $ readIORef boxRef
      liftIO $ writeIORef boxRef $ setInputNumSelection n box
      liftIO $ widgetQueueDraw area
      return False
    
   on outputNum changeValue    $ \a n -> do
      box <- liftIO $ readIORef boxRef
      liftIO $ writeIORef boxRef $ setOutputNumSelection n box
      liftIO $ widgetQueueDraw area
      return False
-}
   on mainWindow objectDestroy mainQuit
   widgetShowAll mainWindow

   mainGUI

gateMod old new = do
   x <- eventModifier
   return $ case x of
      [] -> new
      [Control] -> new /= old

mapSel f sf xs = map (\x -> if sf x then f x else x) xs

orf all f x = all || f x

colourSel True  = setSourceRGB 1 0 0
colourSel False = setSourceRGB 0 0 0