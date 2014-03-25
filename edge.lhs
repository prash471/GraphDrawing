>import Diagrams.Prelude
>import Diagrams.Backend.SVG.CmdLine

>sPt = p2 (0.20, 0.20)
>ePt = p2 (2.85, 0.85)
>edge = (sPt, ePt)
>dot  = circle 0.02 # lw 0
>
> -- We use small blue and red circles to mark the start and end points.
>sDot = dot # fc blue # moveTo sPt
>eDot = dot # fc red  # moveTo ePt
>example = ( sDot <> eDot <> arrowBetween (fst edge) (snd edge))
>   		     # centerXY # pad 1.1
>main = mainWith (example :: Diagram B R2)
