module Geometry.Cuboid
( volume
, area
) where

volume :: Float -> Float -> Float -> Float
volume a b c = rectangle a b * c

area :: Float -> Float -> Float -> Float
area a b c = rectangle a b * 2 + rectangle a c * 2 + rectangle c b * 2

rectangleArea :: Float -> Float -> Float
rectangleArea a b = a * b
