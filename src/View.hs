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

-- TODO
colourChoiceToColour :: ColourChoice -> Colour
colourChoiceToColour = undefined

-- TODO
shapeToPicture :: Shape -> Picture
shapeToPicture = undefined

-- TODO
colourShapeToPicture :: ColourShape -> Picture
colourShapeToPicture = undefined

-- TODO
colourShapesToPicture :: [ColourShape] -> Picture
colourShapesToPicture = undefined