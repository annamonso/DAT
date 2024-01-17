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

newTrafficLight :: (Double,Double) -> Drawing
newTrafficLight (x,y) = translated x y (trafficLight)

trafficLights ::  [(Double, Double)] -> Drawing
trafficLights list = foldMap newTrafficLight list

coords = [(-3,-8),(-3,0),(-3,8),(0,-8),(0,0),(0,8),(3,-8),(3,0),(3,8)]

myDrawing :: Drawing
myDrawing = trafficLights coords



main :: IO ()
main = putSvg myDrawing
