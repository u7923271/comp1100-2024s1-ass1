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

-- TODO
toolToLabel :: Tool -> String
toolToLabel = undefined

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