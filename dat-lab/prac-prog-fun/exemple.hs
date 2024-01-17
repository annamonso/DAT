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

myDrawing :: Drawing
myDrawing = trafficLight

main :: IO ( )
main = putSvg (coordinatePlane <> myDrawing)
