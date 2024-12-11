module Data.Relations where

import Data.Set qualified as S
import Data.List (intercalate)

-- Attributes --

newtype Attribute = Attr { str :: String } deriving (Ord, Eq)

instance Show Attribute where
    show = str

-- Schema --

type Schema = S.Set Attribute

-- Functional Dependencies --

data FunctionalDependency = To (S.Set Attribute) (S.Set Attribute)
    deriving (Ord,Eq)

leftSide :: FunctionalDependency -> S.Set Attribute
leftSide (To lhs _) = lhs

rightSide :: FunctionalDependency -> S.Set Attribute
rightSide (To _ rhs) = rhs

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

isBasis :: Cover -> Bool
isBasis = all ((1 ==) . S.size . rightSide)

-- Relations --

data Relation = Rel Schema Cover
    deriving (Eq)

verifyRelation :: Relation -> Bool
verifyRelation (Rel schema fds) = all (check schema) fds
    where
        check :: Schema -> FunctionalDependency -> Bool
        check schema (To lhs rhs) = S.union lhs rhs `S.isSubsetOf` schema

instance Show Relation where
    show (Rel s c) = show (S.toAscList s) ++ ": " ++ display c
        where
            wrap x = "(" ++ x ++ ")"
            display c = wrap (intercalate " | " (map show $ S.toAscList c))