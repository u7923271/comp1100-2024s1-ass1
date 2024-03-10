--- Copyright 2024 The Australian National University, All rights reserved

module Controller where

import CodeWorld
import Model

import Data.Text (pack, unpack)

-- | Compute the new Model in response to an Event.
handleEvent :: Event -> Model -> Model
handleEvent event (Model shapes colour tool) =
  case event of
    KeyPress key
      -- revert to an empty canvas
      | k == "Esc" -> startModel

      -- write the current model to the console
      | k == "D" -> CodeWorld.trace (pack (show inputModel)) inputModel

      -- display the sample images
      | k == "1" -> Model sample1 colour tool

      | k == "2" -> Model sample2 colour tool

      -- TODO
      | k == "Backspace" || k == "Delete" -> undefined

      -- TODO
      | k == " " -> undefined

      -- TODO
      | k == "C" -> undefined

      -- TODO
      | k == "T" -> undefined

      -- ignore other keys
      | otherwise -> inputModel
      
      where
        k = unpack key

    -- TODO
    PointerPress p -> undefined
   
    -- TODO
    PointerRelease p -> undefined
          
    -- ignore other events
    _ -> inputModel
    
    where
      inputModel :: Model
      inputModel = Model shapes colour tool

-- TODO
switchColour :: ColourChoice -> ColourChoice
switchColour = undefined

-- TODO
switchTool :: Tool -> Tool
switchTool = undefined