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

repeatDraw :: (Int->Drawing) -> Int -> Drawing
repeatDraw thing 0 = blank
repeatDraw thing n = thing n <> (repeatDraw thing (n-1))

myDrawing :: Drawing
myDrawing = translated (-8) (-16) (repeatDraw lightRow 3)

lightRow :: Int -> Drawing
lightRow r = repeatDraw (lightf r) 5

lightf:: Int -> Int -> Drawing
lightf r c = translated (3 * fromIntegral c) (8 * fromIntegral r) trafficLight 

main :: IO ()
main = putSvg myDrawing
