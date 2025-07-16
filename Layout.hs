module Layout where

-- Example of using a PangoLayout
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Cairo
import Graphics.UI.Gtk.Gdk.EventM
import Graphics.Rendering.Cairo

loremIpsum = "REVOLUTION"

main = do
  initGUI
  -- Create the main window.
  win <- windowNew
  on win objectDestroy mainQuit
  -- Create a drawing area in which we can render text.
  area <- drawingAreaNew
  containerAdd win area
  on area sizeRequest $ return (Requisition 100 100)

  -- Create a Cairo Context that contains information about the current font,
  -- etc.
  ctxt <- cairoCreateContext Nothing
  lay <- layoutText ctxt loremIpsum
  layoutSetWrap lay WrapWholeWords

  -- Wrap the layout to a different width each time the window is resized.
  on area sizeAllocate $ \(Rectangle _ _ w _) -> do
    layoutSetWidth lay (Just (fromIntegral w))

  -- Setup the handler to draw the layout.
  on area exposeEvent $ updateArea area lay

  -- Run the whole thing.
  widgetShowAll win
  mainGUI

updateArea :: DrawingArea -> PangoLayout -> EventM EExpose Bool
updateArea area lay = do
  win <- eventWindow
  liftIO $
    renderWithDrawWindow win $ do
      moveTo 0 0
      showLayout lay

  return True
