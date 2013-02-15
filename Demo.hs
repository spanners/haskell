import Data.IORef
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Gdk.GC(gcNew)
import Graphics.UI.Gtk.Gdk.Events(eventX,eventY)

main = do
  initGUI
  
  pointsRef <- newIORef []
    
  widthRef <- newIORef 1
  
  win <- windowNew
  onDestroy win $ do
    mainQuit
    
  vBox <- vBoxNew False 0
  
  containerAdd win vBox
  
  
  da <- drawingAreaNew
  onButtonPress da $ \e -> do
    points <- readIORef pointsRef
    let points' = (round (eventX e), round (eventY e)) : points
    writeIORef pointsRef points'
    print points'
    widgetQueueDraw da
    return True
  
  onExpose da $ \e -> do
    points <- readIORef pointsRef
    width <- readIORef widthRef
    dw <- widgetGetDrawWindow da
    gc <- gcNew dw 
    gcVals <- gcGetValues gc
    gcSetValues gc gcVals{lineWidth=width}
    drawLines dw gc points
    return True
    
  onSizeRequest da $ do  
    return (Requisition 300 200)
  boxPackStart vBox da PackGrow 0
    
  hBox <- hBoxNew False 0
  boxPackStart vBox hBox PackNatural 0
  
  entry <- entryNew
  boxPackStart hBox entry PackGrow 0
  
  btn <- buttonNewWithLabel "Set Width"
  onClicked btn $ do 
    s <- entryGetText entry
    case read s of
      [(width,"")] -> do writeIORef widthRef width
      _            -> error "????"
  boxPackStart hBox btn PackNatural 0
  
  widgetShowAll win
    
  mainGUI