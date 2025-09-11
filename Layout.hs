{-# LANGUAGE ViewPatterns #-}

module Layout where

-- Example of using a PangoLayout
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Cairo
import Graphics.UI.Gtk.Gdk.EventM
import Graphics.Rendering.Cairo


import Codec.Picture
    
loremIpsum = "REVOLUTION"

main = do
  initGUI
  -- Create the main window.
  win <- windowNew
  on win objectDestroy mainQuit
  -- Create a drawing area in which we can render text.
  vb  <- vBoxNew False 0
  area <- drawingAreaNew
  boxPackStart vb area PackGrow 0
  --on area sizeRequest $ return (Requisition 1000 1000)

  -- Create a Cairo Context that contains information about the current font,
  -- etc.
  font <- fontDescriptionNew
  fontDescriptionSetFamily font "Sans"
  fontDescriptionSetSize font 72
  ctxt <- cairoCreateContext Nothing
  lay <- layoutText ctxt loremIpsum
  layoutSetWrap lay WrapWholeWords
  layoutSetFontDescription lay (Just font)
  -- Wrap the layout to a different width each time the window is resized.
  on area sizeAllocate $ \(Rectangle _ _ w _) -> do
    layoutSetWidth lay (Just (fromIntegral w))


  -- Setup the handler to draw the layout.
  on area draw $ updateArea lay

  -- Run the whole thing.
  containerAdd win vb
  widgetShowAll win
  mainGUI

updateArea :: PangoLayout -> Render ()
updateArea lay = do
  setSourceColor $ Color 65535 0 0
  setSourceRGB 1 0 0
  moveTo 0 0
  showLayout lay

