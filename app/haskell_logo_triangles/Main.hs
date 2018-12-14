module Main (main) where



import Control.Monad.Trans.State
import Data.Foldable
import Graphics.Rendering.Cairo  hiding (transform, x, y)
import System.Random

import Comparison
import Draw
import Geometry.Core
import Geometry.Processes.RandomCut
import Geometry.Shapes
import Geometry.Triangulate



picWidth, picHeight :: Num a => a
picWidth = 500
picHeight = 360

haskellLogo' :: [Polygon]
haskellLogo' = transform (scale' 340 340) haskellLogo

main :: IO ()
main = png >> svg
  where
    png = do
        surface <- createImageSurface FormatARGB32 picWidth picHeight
        renderWith surface drawing
        surfaceWriteToPNG surface "out/haskell_logo_triangles.png"
    svg = withSVGSurface "out/haskell_logo_triangles.svg" picWidth picHeight (\surface -> renderWith surface drawing)

shatterProcessS
    :: (Polygon -> Bool)   -- ^ Recursively subdivide the current polygon?
    -> ([Polygon] -> Bool) -- ^ Accept the cut result, or retry with a different random cut line?
    -> Polygon             -- ^ Initial polygon, cut only if the recursion predicate applies
    -> State StdGen [Polygon]
shatterProcessS recurse acceptCut polygon
    | recurse polygon = do
        cutPieces <- randomCutS acceptCut polygon
        let triangulated = concatMap triangulate cutPieces
        pure triangulated
        -- if acceptCut triangulated
        --     then fmap concat (traverse (shatterProcessS recurse acceptCut) triangulated)
        --     else shatterProcessS recurse acceptCut polygon
    | otherwise = pure [polygon]

runShatterProcess
    :: (Polygon -> Bool)
    -> ([Polygon] -> Bool)
    -> [Polygon]
    -> StdGen
    -> [Polygon]
runShatterProcess recurse acceptCut initialPolygon gen
  = evalState (fmap concat (traverse (shatterProcessS recurse acceptCut) initialPolygon)) gen

drawing :: Render ()
drawing = do
    let addArea (Area a) (Area b) = Area (a + b)
        totalArea = foldl' (\acc poly -> acc `addArea` polygonArea poly) (Area 0) haskellLogo'
        recurse polygon = let Area a = polygonArea polygon
                              Area b = totalArea
                          in a >= b/64
        acceptCut polygons = minMaxAreaRatio polygons >= 1/3
        shattered = runShatterProcess recurse acceptCut haskellLogo' (mkStdGen 16)
    translate 10 10
    setLineCap LineCapRound
    setLineJoin LineJoinRound
    restoreStateAfter $ for_ shattered $ \polygon -> do
        let gen = let Area a = polygonArea polygon
                      (x,y) = decodeFloat a
                  in mkStdGen (fromIntegral x + y)
            (hue, gen1) = randomR (200, 215) gen
            (saturation, gen2) = randomR (0.6, 0.9) gen1
            (value, _gen3) = randomR (0.6, 1.0) gen2
            alpha = 1
        hsva hue saturation value alpha
        polygonSketch polygon
        fillPreserve
        stroke
    restoreStateAfter $ for_ haskellLogo' $ \polygon -> do
        polygonSketch polygon
        setLineJoin LineJoinRound
        hsva 0 0 0 1
        stroke
