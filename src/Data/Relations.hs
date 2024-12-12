module Data.Relations where

import qualified Data.Set as S
import Data.List (intercalate)

-- Attributes --

newtype Attribute = Attr { str :: String } deriving (Ord, Eq)

instance Show Attribute where
    show = str

-- Schemata --

type Schema = S.Set Attribute

-- Functional Dependencies --

data FunctionalDependency = To { leftSide :: S.Set Attribute, rightSide :: S.Set Attribute }
    deriving (Ord,Eq)

attributesOf :: FunctionalDependency -> S.Set Attribute
attributesOf x = S.union (leftSide x) (rightSide x)

instance Show FunctionalDependency where
    show (To l r) = display l ++ "->" ++ display r
        where
            display :: S.Set Attribute -> String
            display s
              | all ((== 1) . length . str) s = str =<< S.toAscList s
              | otherwise = intercalate "," (map str $ S.toAscList s)

-- Covers --

type Cover = S.Set FunctionalDependency

-- returns whether a given cover is also a basis: whether
-- it only contains FDs with one attribute on their right side
isBasis :: Cover -> Bool
isBasis = all ((1 ==) . S.size . rightSide)

-- Relations --

data Relation = Rel Schema Cover
    deriving (Eq)

-- verifies a Relation meets its representation invariant - that is,
-- each FD in the relation's cover only uses attributes of the relation
verifyRelation :: Relation -> Bool
verifyRelation (Rel schema fds) = all ((`S.isSubsetOf` schema) . attributesOf) fds

instance Show Relation where
    show (Rel s f) = show (S.toAscList s) ++ ": " ++ display f
        where
            wrap x = "(" ++ x ++ ")"
            display c = wrap (intercalate " | " (map show $ S.toAscList c))