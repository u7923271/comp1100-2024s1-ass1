--- Copyright 2024 The Australian National University, All rights reserved

module View where

import CodeWorld
import Data.Text (pack)
import Model

-- | Render all the parts of a Model to a picture.
-- | You do not need to understand all parts of this function.
modelToPicture :: Model -> Picture
modelToPicture (Model shapes colour tool)
  = translated 0 7 colourText
  &  translated 0 8 toolText 
  & colourShapesToPicture shapes
  & coordinatePlane
  where
    colourText :: Picture
    colourText = stringToText (show colour)

    toolText :: Picture
    toolText = stringToText (toolToLabel tool)

    stringToText = lettering . pack

-- DONE!
toolToLabel :: Tool -> String
toolToLabel tool = case tool of
  LineTool -> "Line: click-drag-release"
  PolyTool -> "Polygon: click 3+ times, then spacebar"
  RectangleTool -> "Rectangle: click-drag-release for opposite corners"
  CircleTool -> "Circle: click-drag-release from centre to perimeter"
  TriangleTool -> "Triangle: click-drag-release from centre to corner"
  CuboidTool -> "Cuboid: click-drag-release for opposite corners, then click new top-left corner"

-- DONE!
colourChoiceToColour :: ColourChoice -> Colour
colourChoiceToColour cols = case cols of
  Red -> red
  Green -> green
  Blue -> blue
  Cyan -> RGBA !0 !255 !255 !1
  Magenta -> RGBA !255 !0 !255 !1
  Yellow -> yellow
  White -> white
  Black -> black

slope :: Point -> Point -> Double 
slope a b = (abs ((abs (y-b))/(abs (x-a))))

-- TODO
shapeToPicture :: Shape -> Picture
shapeToPicture pic = case pic of
  (Line (x,y) (x,y)) -> drawingOf(polyline[(x,y),(a,b)]) --Done
  (Polygon [x:xs]) -> drawingOf(solidPolygon[x:xs]) --Done
  (Rectangle (x,y) (a,b)) -> drawingOf(solidPolygon[(x,y),(a,y),(a,b),(x,b),(x,y)]) --Done
  (Circle (x,y) (a,b)) -> drawingOf(translate(solidCircle(slope (x,y) (a,b)), x, y)) --Done
  Triangle x y -> 
  (Cuboid (x,y) (a,b) (p,r)) -> 

-- TODO
colourShapeToPicture :: ColourShape -> Picture
colourShapeToPicture colshape = case colshape of
  (Line, x) -> drawingOf(coloured(polyline[(x,y),(a,b)]), x)
  (Polygon, x) -> drawingOf(coloured(solidPolygon[x:xs]), x)
  (Rectangle, x) -> drawingOf(coloured(solidPolygon[(x,y),(a,y),(a,b),(x,b)]), x)
  (Circle, x) -> drawingOf(coloured(translate(solidCircle(slope (x,y) (a,b)), x, y)), x)
  (Triangle, x) ->
  (Cuboid, x) -> 

-- DONE!
colourShapesToPicture :: [ColourShape] -> Picture
colourShapesToPicture colshapes = case colshapes of
  [] = drawingOf(blank)
  [x] = colourShapesToPicture x
  [x:xs] = colourShapesToPicture xs