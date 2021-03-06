{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Properties (tests) where



import Control.Applicative
import Text.Printf

import Geometry
import Util

import Test.QuickCheck
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Test.Helpers



tests :: TestTree
tests = testGroup "Properties"
    [ angleBetweenTest
    , areaTest
    , intersectionLLTest
    , lengthOfAngledLineTest
    , detCrossTest
    , dotProductTest
    , polygonInstancesTest
    , transformationTest
     ]

angleBetweenTest :: TestTree
angleBetweenTest = testProperty "Angle between two lines"
    (\angle1@(Angle angle1Raw) angle2@(Angle angle2Raw) ->
        let line1 = angledLine (Vec2 0 0) angle1 (Distance 1)
            line2 = angledLine (Vec2 0 0) angle2 (Distance 1)
            actualAngle = angleBetween line1 line2
            expectedAngleRaw = angle2Raw - angle1Raw
            expectedAngle = rad expectedAngleRaw
        in counterexample
            (unlines ["Counterexample:"
                     , "  Line 1: " ++ show line1 ++ " (angle: " ++ show angle1 ++ ")"
                     , "  Line 2: " ++ show line2 ++ " (angle: " ++ show angle2 ++ ")"
                     , "  Δ actual:   " ++ show actualAngle
                     , "  Δ expected: " ++ show expectedAngle
                     ])
            (expectedAngle ~== actualAngle) )

areaTest :: TestTree
areaTest = testGroup "Area"
    [ parallelogram
    , square ]
  where
    coord = choose (-100, 100 :: Double)
    vec2 = liftA2 Vec2 coord coord

    square = testProperty "Square" (forAll
        ((,,,,) <$> vec2 <*> vec2 <*> vec2 <*> vec2 <*> arbitrary)
        (\(Vec2 x1 y1, Vec2 x2 y2, center, moveVec, angle) ->
            let poly = (move moveVec . rotateAround center angle . Polygon)
                           [Vec2 x1 y1, Vec2 x1 y2, Vec2 x2 y2, Vec2 x2 y1]
                actual = polygonArea poly
                expected = Area (abs ((x2-x1) * (y2-y1)))
            in actual ~== expected ))

    parallelogram = testProperty "Parallelogram" (forAll
        ((,) <$> vec2 <*> vec2)
        (\(v1, v2) ->
            let actual = polygonArea (Polygon [zero, v1, v1 +. v2, v2])
                zero = Vec2 0 0
                Angle rawAngleBetween = angleBetween (Line zero v1) (Line zero v2)
                Distance v1norm = norm v1
                Distance v2norm = norm v2
                expected = Area (abs (v1norm * v2norm * sin rawAngleBetween))
            in actual ~== expected ))

intersectionLLTest :: TestTree
intersectionLLTest = testProperty "Line-line intersection" (forAll
    ((,,,,) <$> vec2 <*> vec2 <*> dist <*> vec2 <*> dist)
    (\(start, end1, dist1, end2, dist2) ->
        let line1 = Line start end1
            line1' = moveRad (angleOfLine line1) dist1 line1
            line2 = Line start end2
            line2' = moveRad (angleOfLine line2) dist2 line2
            (expectedIntersection, _ty) = intersectionLL line1' line2'
        in approxEqualTolerance (Tolerance 1e-8) start expectedIntersection ))
  where
    coord = choose (-100, 100 :: Double)
    vec2 = liftA2 Vec2 coord coord
    dist = fmap Distance (choose (-100, 100))

lengthOfAngledLineTest :: TestTree
lengthOfAngledLineTest = testProperty "Length of angled line" (forAll
    ((,,) <$> vec2 <*> arbitrary <*> arbitrary)
    (\(start, angle, Positive len) ->
        let actual = lineLength (angledLine start angle (Distance len))
            expected = Distance len
        in actual ~== expected ))
  where
    coord = choose (-100, 100 :: Double)
    vec2 = liftA2 Vec2 coord coord

detCrossTest :: TestTree
detCrossTest = testGroup "Determinant/cross product"
    [ testCase "Positive" (assertBool "Positive" (det (Vec2 1 0) (Vec2 0   1)  > 0))
    , testCase "Negative" (assertBool "Negative" (det (Vec2 1 0) (Vec2 0 (-1)) < 0))
    , testProperty "Antisymmetric" $ \v1 v2 ->
        let Vec2 x1 y1 = v1
            Vec2 x2 y2 = v2
            detA  = det v1 v2
            detA' = det v2 v1
        in counterexample (printf " det (%f,%f) (%f,%f) =  %f,\n-det (%f,%f) (%f,%f) = %f"
                                        x1 y1   x2 y2    detA        x2 y2   x1 y1    detA')
                          (detA ~== -detA')
    , testProperty "Invariant: transposition" $ \x1 y1 x2 y2 ->
        let detA  = det (Vec2 x1 y1) (Vec2 x2 y2)
            detAT = det (Vec2 x1 x2) (Vec2 y1 y2)
        in counterexample (printf "det (%f,%f) (%f,%f) = %f,\ndet (%f,%f) (%f,%f) = %f"
                                        x1 y1   x2 y2    detA      x1 x2   y1 y2    detAT)
                          (detA ~== detAT)
    ]

dotProductTest :: TestTree
dotProductTest = testGroup "Dot product"
    [ testProperty "Commutativity" (\v1 v2 -> dotProduct v1 v2 ~== dotProduct v2 v1)
    , testProperty "Zero is eliminator" (\v1 -> dotProduct v1 (Vec2 0 0) ~== 0)
    , testGroup "Measure of colinearity"
        [ testProperty "…with unit vector" $ \vGen ->
            let v1 = Vec2 1 0
                Vec2 x _ = vGen
            in signum (dotProduct v1 vGen) == signum x
        , testProperty "…with arbitrary vectors" $
            let nonZeroVec = do
                    (NonZero a, b, xy) <- arbitrary
                    pure (if xy then Vec2 a b else Vec2 b a)
            in forAll ((,) <$> nonZeroVec <*> nonZeroVec) $ \(v1, v2) ->
                let v0 = Vec2 0 0
                    angle@(Angle a) = angleBetween (Line v0 v1) (Line v0 v2)
                    prod = dotProduct v1 v2
                in -- Exclude almost-90° angles to avoid blinker tests
                    abs (cos a) >= 1e-10 ==> counterexample
                    (printf "%s • %s = %f\nangle; cos = %s; %.2f"
                            (show v1) (show v2) prod (show angle) (cos a))
                    (signum (cos a) == signum prod)
        ]
    ]

polygonInstancesTest :: TestTree
polygonInstancesTest = testGroup "Eq Polygon"
    [ equalPolygonsTest
    ]
  where
    equalPolygonsTest = testProperty "Polygons with rotated corner order" (forAll gen test)
      where
        gen = do
            size <- getSize
            len <- fmap (+3) (choose (0, size))
            polygon <- fmap Polygon (Test.QuickCheck.vectorOf len arbitrary)
            rot <- choose (0, len-1)
            pure (rot, polygon)
        test (rot, polygon)
          = let Polygon corners = polygon
                polygon' = Polygon (rotate rot corners)
            in polygon === polygon'

transformationTest :: TestTree
transformationTest = testGroup "Affine transformations"
    [ testGroup "Algebraic properties"
        [ testProperty "Multiple rotations add angles" $ \a1@(Angle a1') a2@(Angle a2') ->
            approxEqualTolerance (Tolerance 1e-5)
                (transformationProduct (rotate' a1) (rotate' a2))
                (rotate' (Angle (a1' + a2')))
        ]
    , testGroup "Invertibility"
        [ invertibilityTest "Identity"
            (pure [identityTransformation])
        , invertibilityTest "Translation"
            (do x <- choose (-1000,1000); y <- choose (-1000,1000); pure [translate' x y])
        , invertibilityTest "Scaling"
            (do let sign = elements [1,-1]
                    factor = choose (0.1,10)
                xSign <- sign
                ySign <- sign
                xScale <- factor
                yScale <- factor
                pure [scale' (xSign*xScale) (ySign*yScale)])
        , invertibilityTest "Rotation"
            (do angle <- arbitrary; pure [rotate' angle])
        , invertibilityTest "Combination of transformations" $ do
            size <- getSize
            n <- choose (2, min size 10)
            Test.Tasty.QuickCheck.vectorOf n (frequency
                [ (1, pure identityTransformation)
                , (3, rotate' <$> arbitrary)
                , (3, translate' <$> choose (-100,100) <*> choose (-100,100))
                , (3, scale' <$> liftA2 (*) (elements [-1,1]) (choose (0.2, 5))
                             <*> liftA2 (*) (elements [-1,1]) (choose (0.2, 5))) ])
        ]
    ]
  where
    invertibilityTest name trafosGen
      = let gen = do
                trafos <- trafosGen
                let transformation = foldr transformationProduct identityTransformation trafos
                vec <- arbitrary
                pure (vec :: Vec2, transformation)
            test (vec, transformation) = approxEqualTolerance (Tolerance 1e-5)
                ((transform (inverseTransformation transformation) . transform transformation) vec)
                vec
        in testProperty name (forAll gen test)
