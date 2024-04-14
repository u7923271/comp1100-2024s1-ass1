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

-- DONE
shapeToPicture :: Shape -> Picture
shapeToPicture pic = case pic of
  (Line (x,y) (a,b)) -> drawingOf(polyline[(x,y),(a,b)]) --Done
  (Polygon [x:xs]) -> drawingOf(solidPolygon[x:xs]) --Done
  (Rectangle (x,y) (a,b)) -> drawingOf(solidPolygon[(x,y),(a,y),(a,b),(x,b),(x,y)]) --Done
  (Circle (x,y) (a,b)) -> drawingOf(translated((solidCircle(slope (x,y) (a,b)), x, y))) --Done
  (Triangle (x,y) (a,b)) -> 
  (Cuboid (x,y) (a,b) (p,r)) -> drawingOf((polyline[(x,y),(a,y),(a,b),(x,b),(x,y)])
  &(polyline[(a,b),(p,r),((abs(p-x)),(abs(r-b))),(x,b)])
  &(polyline[(x,y),((abs(p-x)),(abs(b-y))), ((abs(p-x)),(abs(r-b)))])
  &(polyline[(a,y),((abs(p-a)),(abs(r-y))),(p,r)])
  &(polyline[((abs(p-x)),(abs(b-y))),((abs(p-a)),(abs(r-y)))]))

-- DONE
colourShapeToPicture :: ColourShape -> Picture
colourShapeToPicture colshape = case colshape of
  ((Line (x,y) (a,b)), n) -> drawingOf(coloured((polyline[(x,y),(a,b)]), n))
  ((Polygon [x:xs]), n) -> drawingOf(coloured((solidPolygon[x:xs]), n))
  ((Rectangle (x,y) (a,b)), n) -> drawingOf(coloured((solidPolygon[(x,y),(a,y),(a,b),(x,b)]), n))
  ((Circle (x,y) (a,b)), n) -> drawingOf(coloured((translated((solidCircle((slope (x,y) (a,b)))), x, y)), n)) 
  ((Triangle (x,y) (a,b)), n) -> 
  ((Cuboid (x,y) (a,b) (p,r)), n) -> drawingOf(coloured((solidPolygon[(x,y),(x,b),((abs(p-x)),(abs(r-b))),(p,r),(abs(p-a)),(abs(r-y)),(a,y),(x,y)]), n))

-- DONE!
colourShapesToPicture :: [ColourShape] -> Picture
colourShapesToPicture colshapes = case colshapes of
  [] = drawingOf(blank)
  [x] = colourShapesToPicture x
  [x:xs] = colourShapesToPicture xs