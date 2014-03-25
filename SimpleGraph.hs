module Main where

import Graphics.Rendering.Cairo.SVG
import Control.Applicative
import Control.Exception
import Control.Monad
import Data.IORef
import Data.Trees.KdTree as KD
import Graphics.UI.Gtk hiding ((:=))
-- import Graphics.UI.Gtk.Gdk.Events
import Graphics.UI.Gtk.SourceView
import IPPrint
import System.Directory
import System.IO
import Text.Read
import qualified Data.ByteString as B
import qualified Data.Text.Encoding as E
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.IO as L

import qualified Diagrams.BoundingBox as BB

import Diagrams.Prelude hiding (origin)
import Diagrams.Core as C hiding (origin)
import Control.Monad.Trans

import Diagrams.Backend.Cairo hiding (x,y)
import Diagrams.Backend.Cairo.Internal
import Paths_psim
import SvgParse
import ReadBuilderTH
import Data.Char
import System.FilePath
import Control.Lens

import Data.Colour.Names as Colour

import System.Directory

import Control.Monad.Loops

import qualified Data.Graph.Inductive as G

import FglLens
import Types
import HListExtras hiding ((.-.))

import Schematics

main = do
  initGUI

  [pun| window1 button1 drawingarea1 eventbox1 textview1 |]
    <- $(readBuilderTH id "data/gv.glade") return
        -- (getDataFileName . takeFileName)

  wh <- newIORef (0,0)
  dia  <- newIORef (distil :: Diagram B R2)

  on drawingarea1 draw $ do
      h <- liftIO $ widgetGetAllocatedHeight drawingarea1
      w <- liftIO $ widgetGetAllocatedWidth drawingarea1
      liftIO $ writeIORef wh (w,h)
      dia <- liftIO (readIORef dia)
      snd $ renderDia Cairo
            (CairoOptions "unused.png" (Dims (fromIntegral w) (fromIntegral h)) PNG False)
            dia
  on drawingarea1 buttonPressEvent $ tryEvent $ do
    LeftButton <- eventButton
    (x,y) <- eventCoordinates

    liftIO $ do
      (fromIntegral -> w, fromIntegral -> h) <- readIORef wh
      dia0 <- readIORef dia

      let padW = max 0 (w - h*(wU-wL) / (hU-hL))
          padH = max 0 (h - w*(hU-hL) / (wU-wL))
          Just (wL,wU) = extentX dia0
          Just (hL,hU) = extentY dia0
          pp = p2 ( (x - padW/2) / w * (wU-wL) + wL,
                    (h - padH/2 - y) / h * (hU-hL) + hL)
      print (width dia0, height dia0, pp)
      print $ sample dia0 pp 
      return ()
  widgetShowAll window1
  on window1 objectDestroy mainQuit
  mainGUI

dispSprite (n,r) = translate (r^.x) (r^.y) (r^.svg)



dummyKDP px py = x .=. px .*.
  y .=. py .*.
  w .=. 0 .*.
  h .=. 0 .*.
  objectRef .=. error "dummyKDP" .*.
  emptyRecord :: KDP

exampleFlowsheet svg = 
 (([(0,
     x .=. 0 .*.
     y .=. 0 .*.
     base),
   (1,
     x .=. 2*fromIntegral w .*.
     y .=. 0 .*.
     base) ],
     []) ^. from nodeEdge)
 `asTypeOf` (undefined :: G.Gr a ())

  where
  (w,h) = svgGetSize svg
  base = [pun| svg w h |]

inPT (px,py) = do
  kd <- use (flowsheet . flowsheetKD)
  (ox,oy) <- use origin
  case KD.nearestNeighbor kd (dummyKDP (px-ox) (py-oy)) of
    Just [pun| x y w h objectRef |]
        | floor (abs (px-x-ox)) < div w 2,
          floor (abs (py-y-oy)) < div h 2 -> return (Just objectRef)
    _ -> return Nothing


panningVariant
  = flowsheet .=. Proxy .*.
    origin .=. (Proxy :: Proxy (Float,Float)) .*.
    emptyRecord


-- initial state
state0 =
     size .=. (w .=. 640 .*. h .=. 480 .*. emptyRecord) .*.
     src .=. Nothing .*.
     cursorPos .=. (0,0) .*. -- has origin subtracted already
     origin .=. (0,0) .*. -- centre of the window is this
                          -- (x,y) coordinate in the coordinate
                          -- system used for cursorPos and flowsheet
     panning .=. Nothing .*.
     arrowWH .=. (10,10) .*.
     emptyRecord


{-
-- | arrow p1 p2 (w,h) draws an arrow from p1 to p2 with a
-- triangular arrowhead with width w and height h at p2.
arrow (x1,y1) hd@(x2,y2) (w,h) = Line [(x1,y1),(x2,y2)] <>
  Polygon [hd, hd+d1, hd+d2 ] 
  where (dx,dy) = normalize (x2-x1,y2-y1)
        (dx',dy') = (-dy, dx)
        d1 = (-h*dx + w*dx', -h*dy + w*dy')
        d2 = (-h*dx - w*dx', -h*dy - w*dy')
        normalize (a,b) = let n = sqrt (a^2 + b^2) in (a/n, b/n)
        -}




{-
eventHandler event = case event of
  EventResize (w,h) -> size .= [pun| w h |]

  EventKey (SpecialKey KeySpace) Down _ _ -> do
    src .= Nothing

  EventKey (MouseButton LeftButton) Down (Modifiers { shift = Down }) pt@(px,py) -> do
    pt' <- inPT pt
    pn' <- use panning
    case (pt',pn') of
      (_, Just _) ->
        panning .= Nothing
      (Nothing,_) ->
        panning .= Just (\(x,y) -> mkVariant
                              (toLabel origin)
                              (x-px,y-py)
                              panningVariant)
      (Just n,_) -> do
        Just nv <- uses flowsheet (`G.lab` n)
        fs0 <- use flowsheet
        let -- update the nv (node value) xy coordinates given the
            -- new mouse coordinates (x' y')
            panningFn (x',y') = fs0 & match n . nodeValue %~ \nv' ->
                          nv' & x .~ (x' + nv^.x - px)
                              & y .~ (y' + nv^.y - py)
        panning .= Just (\xy -> mkVariant
                                  (toLabel flowsheet)
                                  (panningFn xy)
                                  panningVariant)

  EventKey (MouseButton LeftButton) Up (Modifiers { shift = Up }) pt -> do
    panning .= Nothing
    pt' <- inPT pt
    src' <- use src
    lift $ print (src', pt')
    do -- some debugging
      fs <- use flowsheet
      o <- use origin
      lift $ print o
      lift $ print (fs ^.. nodeEdge . _1 . folded . _2 . x)
      lift $ print (fs ^.. nodeEdge . _1 . folded . _2 . y)
    case (src', pt') of
      (Nothing, Just t) -> src .= Just t
      (Just f, Just t) -> do
          when (f /= t) $ flowsheet %= G.insEdge (f,t, ())
          src .= Nothing
      _ -> return () 

  EventMotion xy -> do
    o <- use origin
    cursorPos .= (xy - o)
    f <- use panning
    for_ f $ modify . setFromVariant . ($ xy)

  _ -> return ()
  -}

worldToPicture w = do return mempty
  {-
  return $ translate (w^.origin) $ mconcat $
    [ -- display nodes
      w^.flowsheet.nodeEdge._1.folded.to dispSprite,

      -- an arrow from src to the cursor
      case nodeIdToXY w =<< w^.src of
        Nothing -> mempty
        Just s -> Color green $ Line [s,w^.cursorPos]] ++

      -- arrows between nodes
      [ Color Colour.red $ arrowBetween (f^.x,f^.y) (t^.x,t^.y) |
          let ln n = w^?flowsheet . match n . nodeValue,
          (ln -> Just f,ln -> Just t,_) <- w^.flowsheet.nodeEdge._2
    ]
    -}

nodeIdToXY w n =
  w^?flowsheet . match n . nodeValue .
      to (\[pun| x y |] -> (x,y) )

{-
    do
        imgexposed <- newIORef Nothing
        writeIORef (renderimg s0) $ do
            ew <- readIORef imgexposed
            case ew of
                Nothing -> return True
                Just ew -> do
                    (w',h') <- liftIO $ widgetGetSize img

                    let render = withSvgFromFile (svgName s0) $ \svg -> do
                        let (w,h) = svgGetSize svg
                            xr = realToFrac w' / realToFrac w
                            yr = realToFrac h' / realToFrac h
                        scale xr yr
                        svgRender svg

                    b <- doesFileExist (svgName s0)
                    if b then renderWithDrawable ew render
                         else return True
        img `on` exposeEvent $ do
            ew <- eventWindow
            liftIO $ writeIORef imgexposed (Just ew)
            liftIO (join $ readIORef (renderimg s0))


    let updateS = do
          b <- doesFileExist (svgName s0)
          when b $ do
            (dd, nl) <- getDimsAndNodeLocations (svgName s0)
            case dd of
                dd:_ -> writeIORef (xywh s0) dd
                _ -> return ()
            writeIORef (nlr s0) (KD.fromList nl)


    either
            (\SomeException{} -> return ())
            (\b -> do
            bb <- textViewGetBuffer (castToTextView txt)
            textBufferSetByteString bb b)
          =<< try (B.readFile (txtName s0))
    updateS

    -- click on elipses to get the name
    onButtonPress ev $ \ev -> case ev of
        Button { eventClick = SingleClick,
                 eventX = x, eventY = y } -> do
                nlr <- readIORef (nlr s0)
                xywh@(xx,yy,ww,hh) <- readIORef (xywh s0)
                (w',h') <- liftIO $ widgetGetSize img
                let xr = x / fromIntegral w' * ww
                    yr = (1 - y / fromIntegral h')*hh  :: Double
                print (getTitle nlr (xr,-yr))
                return True
        _ -> return True

    onClicked b1 $ do
        b <- textViewGetBuffer (castToTextView txt)
        i1 <- textBufferGetStartIter b
        i2 <- textBufferGetEndIter b
        bt <- textBufferGetByteString b i1 i2 False

        B.writeFile (txtName s0) bt

        -- catch exception & show it...
        let dg :: DotGraph String
            dg = parseDotGraph (L.fromChunks [E.decodeUtf8 bt])

        catch (runGraphviz dg Svg (svgName s0) >> return ())
            $ \ e @(SomeException {}) -> hPutStrLn stderr (show e)

        updateS
        join $ readIORef (renderimg s0)
        return ()

    widgetShowAll window
    mainGUI



-- | title of node if location (x,y) falls inside the ellipses in 'NodeLoc'
getTitle :: KdTree NodeLoc -> (Double,Double) -> Maybe String
getTitle t (x,y) = do
    nn <- nearestNeighbor t (NodeLoc { nl_cx = x, nl_cy = y })
    guard ( ((nl_cx nn - x) / nl_rx nn)^2 +
            ((nl_cy nn - y) / nl_ry nn)^2  <= 1   )
    return (nl_title nn)

-}

{-
findNewest :: [FilePath] -> IO FilePath
findNewest =
-}

{-
main = do
  coolerImg <- svgNewFromFile "data/cooler.svg"
  print $ exampleFlowsheet coolerImg ^. flowsheetKD
  playIO
    (InWindow "psim" (640,480) (0,0))
    white
    5
    (flowsheet .=. exampleFlowsheet coolerImg .*. state0)
    worldToPicture 
    ( \event worldState -> do
        print event
        execStateT (eventHandler event) worldState)
    (\ _ w -> return w)
    -}
