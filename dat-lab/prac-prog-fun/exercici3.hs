module Main where
import Drawing


lightBulb :: Color -> Double -> Drawing
lightBulb c i = colored c (translated 0 (i) (solidCircle 1))

botCircle = lightBulb red (-2.5)
topCircle = lightBulb green (2.5)
middleCircle = lightBulb yellow (0)
frame = colored gray (solidRectangle 2.5 7.5) 
rect = colored black (rectangle 2.5 7.5)

trafficLight = frame <> botCircle <> topCircle <> middleCircle <> rect

lights :: Int -> Drawing
lights 0 = blank
lights n = trafficLight <> translated 3 0 (lights(n-1))



duplicate :: Int -> Drawing -> Drawing
duplicate 0 draw = blank
duplicate n  draw = draw <> translated 0 8 (duplicate(n-1) draw)

myDrawing :: Drawing
myDrawing = duplicate 3 (translated (-6) (-8) (lights 5))


main :: IO ( )
main = putSvg (coordinatePlane <> myDrawing)
