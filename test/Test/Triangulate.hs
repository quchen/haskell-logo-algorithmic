module Test.Triangulate (tests) where



import Data.Foldable
import Graphics.Rendering.Cairo hiding (transform, x, y)
import System.Random

import Draw
import Geometry
import Geometry.Shapes
import Util

import Test.Common
import Test.Helpers
import Test.Tasty
import Test.Tasty.HUnit



tests :: TestTree
tests = testGroup "Polygon triangulation"
    [ testGroup "Visual"
        [ testSquare
        , testRegular9gon
        , testHaskellLogo
        , testSpiral
        ]
    ]

testSquare :: TestTree
testSquare = testCase "Square" (test >> assertions)
  where
    triangulation = triangulate (Polygon [Vec2 0 0, Vec2 100 0, Vec2 100 100, Vec2 0 100])
    test = renderAllFormats 120 120 "test/out/triangulation/1_square" $ do
        translate 10 10
        paintTriangulation triangulation
    assertions = assertEqual "Number of triangles" (4-2) (length triangulation)

testRegular9gon :: TestTree
testRegular9gon = testCase "Regular 9-gon" (test >> assertions)
  where
    numCorners = 9
    triangulation = triangulate (transform (scale' 50 50) (regularPolygon numCorners))
    test = renderAllFormats 120 120 "test/out/triangulation/2_regular_polygon" $ do
        translate 60 60
        paintTriangulation triangulation
    assertions = assertEqual "Number of triangles" (numCorners - 2) (length triangulation)

testHaskellLogo :: TestTree
testHaskellLogo = testCase "Haskell logo" (test >> assertions)
  where
    polygon = wonkyHaskellLogo
    triangulation = map triangulate polygon
    test = renderAllFormats 510 360 "test/out/triangulation/3_haskell_logo" $ do
        translate 10 10
        for_ triangulation (restoreStateAfter . paintTriangulation)
    assertions = assertEqual "Number of triangles"
        (map (\(Polygon corners) -> length corners - 2) polygon)
        (map length triangulation)

    wonkyHaskellLogo :: [Polygon]
    wonkyHaskellLogo = map wigglePoly (transform (scale' 340 340) haskellLogo)
      where
        wigglePoly :: Polygon -> Polygon
        wigglePoly (Polygon corners) = Polygon (map wiggle corners)
        wiggle :: Vec2 -> Vec2
        wiggle v
          = let Seed seed = makeSeed v
                (angle, _gen') = randomR (0, 360) (mkStdGen seed)
            in moveRad (Angle angle) (Distance 10) v

testSpiral :: TestTree
testSpiral = testCase "Spiral" (test >> assertions)
  where
    polygon = spiralPolygon 13 20
    triangulation = triangulate polygon
    test = renderAllFormats 280 260 "test/out/triangulation/4_spiral" $ do
        translate 130 130
        paintTriangulation triangulation
    assertions = assertEqual "Number of triangles"
        (let Polygon corners = polygon in length corners - 2)
        (length triangulation)

nubLines :: [Line] -> [Line]
nubLines = nub' . map normalize
  where
    normalize (Line v1 v2) = Line (min v1 v2) (max v1 v2)

paintTriangulation :: [Polygon] -> Render ()
paintTriangulation triangulation = do
    let setColors = map (\c -> mmaColor c) [0..]
    for_ (zip3 [1::Int ..] setColors triangulation) $ \(i, setColor, polygon) -> do
        restoreStateAfter $ do
            polygonSketch polygon
            setColor 0.5
            fill
        restoreStateAfter $ do
            moveToVec (polygonAverage polygon)
            hsva 0 0 0 1
            showTextAligned HCenter VCenter (show i)
    restoreStateAfter $ do
        setLineWidth 0.5
        hsva 0 0 0 1
        let allEdges = polygonEdges =<< triangulation
            uniqueEdges = nubLines allEdges
        for_ uniqueEdges $ \edge -> do
            lineSketch edge
            stroke
