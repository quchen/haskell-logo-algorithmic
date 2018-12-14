module Geometry.Triangulate (
    triangulate
) where



import Data.List
import Data.Ord

import Geometry.Core
import Util



triangulate :: Polygon -> [Polygon]
triangulate polygon = case clipEar polygon of
    (ear, Nothing) -> [ear]
    (ear, Just rest) -> ear : triangulate rest

isEar :: Polygon -> PolygonOrientation -> [Vec2] -> Bool
isEar candidate parentOrientation forbiddenPoints
  =
    not (any (\r -> pointInPolygon r candidate) forbiddenPoints)
    &&
    parentOrientation == polygonOrientation candidate

-- | Ear-clipping algorithm – find an isolated triangle we can cut off. Meant to
-- be iterated until the input is fully triangulated. Probably terrible
-- performance for large polygons.
clipEar :: Polygon -> (Polygon, Maybe Polygon)
clipEar parentPolygon = go parentPolygon
  where
    parentOrientation = polygonOrientation parentPolygon
    bestEar [] = bugError "No ears to cut off"
    bestEar xs = maximumBy (\(e1,_,_) (e2,_,_) -> comparing polygonArea e1 e2) xs
    onlyEars (candidate, forbiddenPoints, _) = isEar candidate parentOrientation forbiddenPoints

    go lastEar@(Polygon [_,_,_]) = (lastEar, Nothing)
    go polygon@(Polygon (_:_:_:_)) = case (bestEar . filter onlyEars . triples) polygon of
        (ear, _, remainder) -> (ear, Just remainder)
    go _other = error "oh no"

triples :: Polygon -> [(Polygon, [Vec2], Polygon)]
triples (Polygon ps)
  = map (\(p:x:q:rest) -> (Polygon [p,x,q], rest, Polygon (p:q:rest))) (rotations ps)
