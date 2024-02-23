module Fields (
    Field (Field),
    GField,
    EField,
    MagField,
) where

import qualified Vector as V

data Field = Field V.Point V.Point V.Vector -- xld, yld, xru, yru, vec

type GField = Field

type EField = Field

type MagField = Field