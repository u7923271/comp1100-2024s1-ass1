--- Copyright 2024 The Australian National University, All rights reserved

module Model where

import CodeWorld

data Shape
  -- | The Line constructor holds the start and end coordinates of the line.
  = Line Point Point
  -- | For the Polygon, a list of vertices.
  | Polygon [Point]
  -- | For the Rectangle, two diagonally opposite vertices.
  | Rectangle Point Point
  -- | For the Circle, the centre and a point on the perimeter.
  | Circle Point Point
  -- | For the (equilateral) Triangle, the centre and one of the vertices.
  | Triangle Point Point
  -- | For the Cuboid, two diagonally opposite vertices on the first rectangle,
  -- | then the top left vertex of the second rectangle.
  | Cuboid Point Point Point
  deriving (Show, Eq) -- You are not expected to understand this line.

data ColourChoice
  = Red
  | Green
  | Blue
  | Cyan
  | Magenta
  | Yellow
  | White
  | Black
  deriving (Show, Eq) -- You are not expected to understand this line.

type ColourShape = (Shape, ColourChoice)

data Tool
  = LineTool (Maybe Point)
  | PolyTool [Point]
  | RectangleTool (Maybe Point)
  | CircleTool (Maybe Point)
  | TriangleTool (Maybe Point)
  | CuboidTool (Maybe Point) (Maybe Point)
  deriving (Show, Eq) -- You are not expected to understand this line.

data Model = Model [ColourShape] ColourChoice Tool
  deriving (Show,Eq) -- You are not expected to understand this line.

-- | Starting Model for when CodeWorld first starts up.
startModel :: Model
startModel = Model [] Black (LineTool Nothing)

-- | Sample image 1, by Magnus Clouston (11yo)
sample1 :: [ColourShape]
sample1 = [
  (Polygon [(-6.3,-1.5),(-6,-2.4),(-6.6,-1.75),(-6.7,-1.6),(-8,-1)],Black),
  (Polygon [(-8,-3),(-7.75,-3),(-6.5,-1.75),(-6.5,-1.5)],Cyan),
  (Triangle (-5.75,2) (-6.25,1.75),Blue),
  (Triangle (-6.75,2.75) (-6.75,2.5),Blue),
  (Triangle (-8,2.75) (-7.75,2.5),Blue),
  (Triangle (-6.25,4) (-6.75,3.5),Blue),
  (Triangle (-8.25,4.25) (-7.75,3.75),Blue),
  (Rectangle (-9,5) (-5,1),Black),
  (Circle (5.5,6.5) (6.5,4.75),Yellow),
  (Rectangle (0,-4) (1,-5),Red),
  (Rectangle (-2,-4) (-1,-5),Red),
  (Circle (0.25,3.5) (0.5,3.5),Magenta),
  (Circle (-0.75,3.75) (-0.75,3.5),Magenta),
  (Line (0.15,2.2) (0.6,2.8),Cyan),
  (Line (-0.7,2.75) (0.15,2.2),Cyan),
  (Polygon [(-2,1),(1,1),(2,2),(2,-3),(1,-4),(-2,-4),(-2,1)],Green),
  (Cuboid (-2,4) (1,1) (-1,5),Green)
  ]

-- | Sample image 2, by Adeline Clouston (8yo)
sample2 :: [ColourShape]
sample2 = [
  (Circle (-0.3,0.2) (-0.3,0.4),Cyan),
  (Line (-3,-0.5) (-5,-0.75),Black),
  (Line (-3,0.5) (-5.5,0.5),Black),
  (Line (2,-0.1) (4.5,-0.25),Black),
  (Line (4,1) (4.5,1),Black),
  (Line (2,0.7) (4,1),Black),
  (Line (-0.4,0.1) (0.15,-0.5),Black),
  (Line (-0.3,0.15) (-1.1,-0.5),Black),
  (Triangle (-0.65,-0.35) (-0.55,0),Red),
  (Polygon [(1,-2),(0,-3.75),(-1,-2)],Magenta),
  (Rectangle (-3,-1) (2,-2),Green),
  (Circle (1.1,2) (0.9,2.3),Blue),
  (Circle (-1.7,2.2) (-2,2),Blue),
  (Triangle (-1,4) (-1.1,4.8),Cyan),
  (Triangle (1.9,4) (2,4.9),Cyan),
  (Cuboid (-3,3) (2,-2) (-1,5),Magenta)
  ]