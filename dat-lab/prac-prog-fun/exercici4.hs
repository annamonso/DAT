module Main where
import Drawing

frame = colored black (solidRectangle 0.05 1) 
yelloCircle = colored yellow (solidCircle 0.2)

tree = frame


branches :: Int -> Drawing
branches 0 = blank
branches n = tree <> (translated (-0.15) 1 (rotated (pi/10) (branches(n-1)))) <> (translated (0.15) 1 (rotated (-pi/10) (branches(n-1)))) <> if n == 1 then translated 0 0.5 (yelloCircle) else blank



myDrawing :: Drawing
myDrawing =  branches 8


main :: IO ( )
main = putSvg (coordinatePlane <> myDrawing)
