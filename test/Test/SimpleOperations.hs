module Test.SimpleOperations (tests) where



import Data.Foldable
import Data.List
import Graphics.Rendering.Cairo hiding (transform, x, y)

import Draw
import Geometry

import Test.Common
import Test.Helpers
import Test.Tasty
import Test.Tasty.HUnit



tests :: TestTree
tests = testGroup "Simple operations"
    [ testGroup "Visual"
        [ rotateLineTest
        , perpendicularBisectorTest
        , perpendicularLineThroughTest
        , testGroup "Point in polygon?"
            [ pointInSquareTest
            , pointInConvexTest
            ]
        ]
    , pointInPolygonRegression
    ]

rotateLineTest :: TestTree
rotateLineTest = testCase "Rotate line" test
  where
    test = renderAllFormats 300 90 "test/out/simple_operations/1_rotate_line" $ do
        translate 30 30
        let initialLine = angledLine (Vec2 0 0) (rad 0) (Distance 75)
            rotated = iterate (rotateAround (Vec2 25 0) (deg 20)) initialLine

        setLineWidth 1
        for_ (zip [0..8] rotated) (\(i, line) -> do
            mmaColor i 1
            lineSketch line
            stroke )

        mmaColor 1 1
        setFontSize 12
        moveTo 90 20
        showText "Rotate line in 20° increments"

perpendicularBisectorTest :: TestTree
perpendicularBisectorTest = testCase "Perpendicular bisector" test
  where
    test = renderAllFormats 190 70 "test/out/simple_operations/2_perpendicular_bisector" $ do
        translate 10 20
        let line = angledLine (Vec2 0 0) (deg 30) (Distance 50)
            bisector = perpendicularBisector line

        setLineWidth 1
        mmaColor 0 1
        lineSketch line
        stroke
        mmaColor 1 1
        lineSketch bisector
        stroke

        setFontSize 12
        moveTo 40 10
        showText "Perpendicular bisector"

perpendicularLineThroughTest :: TestTree
perpendicularLineThroughTest = testCase "Perpendicular line through point" test
  where
    test = renderAllFormats 250 70 "test/out/simple_operations/3_line_through" $  do
        translate 10 10
        let line = angledLine (Vec2 0 0) (deg 30) (Distance 50)
            point = Vec2 20 30
            line' = perpendicularLineThrough point line

        setLineWidth 1
        mmaColor 0 1
        lineSketch line
        stroke
        circleSketch point (Distance 3)
        stroke
        mmaColor 1 1
        lineSketch line'
        stroke

        setFontSize 12
        moveTo 40 40
        showText "Perpendicular line through point"

pointInSquareTest :: TestTree
pointInSquareTest = testCase "Square" (render >> assertions)
  where
    polygon = Polygon [Vec2 0 0, Vec2 50 0, Vec2 50 50, Vec2 0 50]
    points = [Vec2 x (0.25*x + 20) | x <- [-15, -5 .. 60] ]
    (inPolygon, outsidePolygon) = partition (\p -> pointInPolygon p polygon) points
    render = renderAllFormats 200 70 "test/out/simple_operations/4_point_in_square" $ do
        translate 30 10

        setLineWidth 1
        polygonSketch polygon
        mmaColor 0 1
        strokePreserve
        mmaColor 0 0.1
        fill

        mmaColor 1 1
        for_ inPolygon $ \point -> do
            circleSketch point (Distance 3)
            fill
        for_ outsidePolygon $ \point -> do
            circleSketch point (Distance 3)
            stroke

        setFontSize 12
        moveTo 60 20
        showText "Point in polygon?"
    assertions = assertEqual
        "Points indide/outside polygon"
        (5, 4)
        (length inPolygon, length outsidePolygon)

pointInConvexTest :: TestTree
pointInConvexTest = testCase "Convex polygon" (render >> assertions)
  where
    discardOutliers = filter (\p -> let Distance d = norm p in d <= 150)
    points = (discardOutliers . take 512 . transform (scale' 75 75)) (gaussianVecs (Seed 0))
    polygon = (convexHull . discardOutliers . take 32 . transform (scale' 50 50)) (gaussianVecs (Seed 4))
    (inPolygon, outsidePolygon) = partition (\p -> pointInPolygon p polygon) points
    render = renderAllFormats 320 320 "test/out/simple_operations/5_point_in_convex" $ do
        setLineWidth 1
        translate 160 160
        restoreStateAfter $ do
            mmaColor 0 1
            for_ (polygonEdges polygon) $ \edge -> do
                lineSketch edge
                stroke
            mmaColor 0 0.1
            polygonSketch polygon
            fill
        restoreStateAfter $ do
            mmaColor 1 1
            for_ inPolygon $ \p -> do
                circleSketch p (Distance 2)
                fill
            for_ outsidePolygon $ \p -> do
                circleSketch p (Distance 2)
                stroke
    assertions = assertEqual
        "Points indide/outside polygon"
        (285,154)
        (length inPolygon, length outsidePolygon)

-- This nasty point was inside the polygon, because the line leading towards it
-- crossed the polygon in three places: once as a normal intersection, and twice
-- as a single intersection with a corner. This led to the false claim that the
-- point was inside the polygon.
pointInPolygonRegression :: TestTree
pointInPolygonRegression = testGroup "Point-in-polygon regressions"
    [ testCase (show n) (regressionTestOutside p polygon)
        | (n, (p, polygon)) <- zip [1..]
            [ (Vec2 40.0 (-60),  Polygon [Vec2 (-80) (-80), Vec2 (-60) 80,    Vec2 (-60) (-60)])
            , (Vec2 0.0 0.0,     Polygon [Vec2 0.0 100.0,   Vec2 100.0 0.0,   Vec2 100.0 100.0])
            , (Vec2 0.0 100.0,   Polygon [Vec2 0.0 0.0,     Vec2 100.0 0.0,   Vec2 100.0 100.0])
            , (Vec2 100.0 0.0,   Polygon [Vec2 0.0 0.0,     Vec2 100.0 100.0, Vec2 0.0 100.0])
            , (Vec2 100.0 100.0, Polygon [Vec2 0.0 0.0,     Vec2 100.0 0.0,   Vec2 0.0 100.0])
            ]
    ]
  where
    regressionTestOutside point polygon = assertBool
        "Point should be outside of the polygon"
        (not (pointInPolygon point polygon))
