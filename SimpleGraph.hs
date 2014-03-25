
import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
dim p q label = (left <> (text label `place` (l 0.5)) <> right)
              # translate (v' ^* 1)
  where
    left  = a (l 0.4) p <> (p .+^ v') ~~ (p .+^ (-v'))
    right = a (l 0.6) q <> (q .+^ v') ~~ (q .+^ (-v'))
    
    a = arrowBetween' (with & headSize .~ 1)
    
    v  = (q .-. p) # normalized
    v' = rotate (-1/4 :: Turn) v
    

    l t = p .+^ (q .-. p) ^* t -- center point

f w h = centerXY (r ||| r)
     <> dim ((-w) ^& (-h/2)) (0 ^& (-h/2)) "x"
     <> dim (  0  ^& (-h/2)) (w ^& (-h/2)) "x"
     <> dim (  w  ^& (-h/2)) (w ^& ( h/2)) "y"
  where
    r = rect w h

example w h = f w h # lw 0.1 # centerXY # pad 1.1
main = mainWith (example 6 6:: Diagram B R2)
