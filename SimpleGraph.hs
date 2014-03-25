>import Diagrams.Prelude
>import Diagrams.Backend.SVG.CmdLine
>node :: Int -> Diagram B R2
>node n = text (show n) # scale 0.2 # fc white
>       <> circle 0.2 # fc green # named n
>
>arrowOpts = with & headGap  .~ 0.07
>                  & tailGap  .~ 0.07
>                  & headSize .~ 0.2
>
>tournament :: Int -> Diagram B R2
>tournament n = decorateTrail (regPoly n 1) (map node [1..n])
>   # applyAll [connectOutside' arrowOpts j k | j <- [1 , 3 , 5], k <- [j+1 .. n]]
>example = tournament 6
>main = mainWith (example :: Diagram B R2)
