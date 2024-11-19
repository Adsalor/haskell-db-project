module Data.Relations where

import Data.Set (Set, toAscList, member, isSubsetOf, union, size)
import Data.List (intercalate,sort,nub)

-- Attributes --

newtype Attribute = Attr { str :: String } deriving (Ord, Eq)

instance Show Attribute where
    show = str

-- Schema --

type Schema = Set Attribute

-- Functional Dependencies --

data FunctionalDependency = To (Set Attribute) (Set Attribute)
    deriving (Ord,Eq)

instance Show FunctionalDependency where
    show (To l r) = display l ++ "->" ++ display r
        where
            display :: Set Attribute -> String
            display s = intercalate "," (map str $ toAscList s)

-- Covers --

type Cover = Set FunctionalDependency

isBasis :: Cover -> Bool
isBasis = all rhs1
    where
        rhs1 :: FunctionalDependency -> Bool
        rhs1 (To l r) = size r == 1

-- Relations --

data Relation = Rel Schema Cover

verifyRelation :: Relation -> Bool
verifyRelation (Rel schema fds) = all (check schema) fds
    where
        check :: Schema -> FunctionalDependency -> Bool
        check schema (To lhs rhs) = union lhs rhs `isSubsetOf` schema

instance Show Relation where
    show (Rel s c) = show s ++ ": " ++ display c
        where
            wrap x = "(" ++ x ++ ")"
            display c = wrap (intercalate "|" (map show $ toAscList c))