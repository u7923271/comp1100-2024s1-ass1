module Main where

import Controller
import Model
import View
import Testing

-- | The list of all tests to run.
tests :: [Test]
tests = switchColourTests ++ switchToolTests ++ toolLabelTests

-- | Tests for switchColour
switchColourTests :: [Test]
switchColourTests =
  [ Test "Red -> Green" (assertEqual (switchColour Red) Green)
  , Test "Green -> Blue" (assertEqual (switchColour Green) Blue)
  , Test "Blue -> Cyan" (assertEqual (switchColour Blue) Cyan)
  , Test "Cyan -> Magenta" (assertEqual (switchColour Cyan) Magenta)
  , Test "Magenta -> Yellow" (assertEqual (switchColour Magenta) Yellow)
  , Test "Yellow -> White" (assertEqual (switchColour Yellow) White)
  , Test "White -> Black" (assertEqual (switchColour White) Black)
  , Test "Black -> Red" (assertEqual (switchColour Black) Red)
  ]

-- | Tests for switchTool,
-- including tests that it does not change tools midway through drawing.
switchToolTests :: [Test]
switchToolTests =
  [ Test "Line -> Polygon"
      (assertEqual (switchTool (LineTool Nothing)) (PolyTool []))
  , Test "Polygon -> Rectangle"
      (assertEqual (switchTool (PolyTool [])) (RectangleTool Nothing))
  , Test "Rectangle -> Circle"
      (assertEqual (switchTool (RectangleTool Nothing)) (CircleTool Nothing))
  , Test "Circle -> Triangle"
      (assertEqual (switchTool (CircleTool Nothing)) (TriangleTool Nothing))
  , Test "Triangle -> Cuboid"
        (assertEqual (switchTool (TriangleTool Nothing)) (CuboidTool Nothing Nothing))
  , Test "Cuboid -> Line"
        (assertEqual (switchTool (CuboidTool Nothing Nothing)) (LineTool Nothing))
  , Test "Line (in use) -> Line"
      (assertEqual (switchTool (LineTool (Just (1,1)))) (LineTool (Just (1,1))))
  , Test "Polygon (in use) -> Polygon"
      (assertEqual (switchTool (PolyTool [(2,2)])) (PolyTool [(2,2)]))
  , Test "Rectangle (in use) -> Rectangle"
      (assertEqual (switchTool (RectangleTool (Just (3,3)))) (RectangleTool (Just (3,3))))
  , Test "Circle (in use) -> Circle"
      (assertEqual (switchTool (CircleTool (Just (4,4)))) (CircleTool (Just (4,4))))
  , Test "Triangle (in use) -> Triangle"
      (assertEqual (switchTool (TriangleTool (Just (5,5)))) (TriangleTool (Just (5,5))))
  , Test "Cuboid (in use) -> Cuboid 1"
      (assertEqual (switchTool (CuboidTool (Just (5,5)) Nothing)) (CuboidTool (Just (5,5)) Nothing))
  , Test "Cuboid (in use) -> Cuboid 2"
      (assertEqual (switchTool (CuboidTool (Just (5,5)) (Just (5.5,5.5)))) (CuboidTool (Just (5,5)) (Just (5.5,5.5))))
  ]

-- | Tests for toolToLabel
toolLabelTests :: [Test]
toolLabelTests =
  [ Test "LineTool 1"
      (assertEqual (toolToLabel (LineTool Nothing))
       "Line: click-drag-release")
  , Test "LineTool 2"
      (assertEqual (toolToLabel (LineTool (Just (1,1))))
       "Line: click-drag-release")
  , Test "PolyTool 1"
      (assertEqual (toolToLabel (PolyTool []))
      "Polygon: click 3+ times, then spacebar")
  , Test "PolyTool 2"
      (assertEqual (toolToLabel (PolyTool [(2,2),(2.2,2.2)]))
      "Polygon: click 3+ times, then spacebar")
  , Test "RectangleTool 1"
      (assertEqual (toolToLabel (RectangleTool Nothing))
      "Rectangle: click-drag-release for opposite corners")
  , Test "RectangleTool 2"
      (assertEqual (toolToLabel (RectangleTool (Just (3,3))))
      "Rectangle: click-drag-release for opposite corners")
  , Test "CircleTool 1"
      (assertEqual (toolToLabel (CircleTool Nothing))
      "Circle: click-drag-release from centre to perimeter")
  , Test "CircleTool 2"
      (assertEqual (toolToLabel (CircleTool (Just (4,4))))
      "Circle: click-drag-release from centre to perimeter")
  , Test "TriangleTool 1"
      (assertEqual (toolToLabel (TriangleTool Nothing))
      "Triangle: click-drag-release from centre to corner")
  , Test "TriangleTool 2"
      (assertEqual (toolToLabel (TriangleTool (Just (4,4))))
      "Triangle: click-drag-release from centre to corner")
  , Test "ParallelogramTool 1"
      (assertEqual (toolToLabel (CuboidTool Nothing Nothing))
      ("Cuboid: click-drag-release for opposite corners, then click new top-left corner"))
  , Test "ParallelogramTool 2"
      (assertEqual (toolToLabel (CuboidTool (Just (5,5)) (Just (5.5,5.5))))
      ("Cuboid: click-drag-release for opposite corners, then click new top-left corner"))
  ]

-- | A Haskell program starts by running the computation defined by
-- 'main'. We run the list of tests that we defined above.
main :: IO ()
main = runTests tests