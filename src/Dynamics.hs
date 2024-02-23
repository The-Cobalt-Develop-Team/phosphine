module Dynamics (
    toAddition,
    fromGField,
    fromEField,
    fromMagField,
    Item (Item),
) where

import qualified Fields as F
import Vector (Point)
import qualified Vector as V

type Velocity = V.Vector

type Force = V.Vector

data Item = Item Rational Rational Point Velocity Force

toAddition :: Force -> Rational -> Rational -> Velocity
toAddition f m step = f V./ m V.* step

fromGField :: F.GField -> Point -> Rational -> Force
fromGField (F.Field pt1 pt2 vec) pt m =
    if V.isInside pt1 pt2 pt
        then vec V.* m
        else V.vZero vec

fromEField :: F.EField -> Point -> Rational -> Force
fromEField (F.Field pt1 pt2 vec) pt q =
    if V.isInside pt1 pt2 pt
        then vec V.* q
        else V.vZero vec

fromMagField :: F.MagField -> Point -> Velocity -> Rational -> Force
fromMagField (F.Field pt1 pt2 vec) pt v q =
    if V.isInside pt1 pt2 pt
        then (v V.* q) `V.vInner` vec
        else V.vZero (v V.* q) `V.vInner` vec
