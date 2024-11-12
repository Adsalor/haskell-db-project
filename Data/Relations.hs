module Data.Relations where

import Data.List (intercalate,sort,nub)

-- Attributes --

newtype Attribute = Attr { str :: String } deriving (Ord, Eq)

instance Show Attribute where
    show = str

-- Schema --

type Schema = [Attribute]

verifySchema :: Schema -> Bool
verifySchema x = length x == length (nub x)

-- Functional Dependencies --

data FunctionalDependency = To [Attribute] [Attribute]

instance Show FunctionalDependency where
    show (To l r) = display l ++ "->" ++ display r
        where
            display :: [Attribute] -> String
            display s = intercalate "," (map str $ sort s)

-- Covers --

type Cover = [FunctionalDependency]

isBasis :: Cover -> Bool
isBasis = all rhs1
    where
        rhs1 :: FunctionalDependency -> Bool
        rhs1 (To l r) = length r == 1

-- Relations --

data Relation = Rel Schema Cover

verifyRelation :: Relation -> Bool
verifyRelation (Rel schema fds) = verifySchema schema && all (check schema) fds
    where
        check :: Schema -> FunctionalDependency -> Bool
        check schema (To lhs rhs) = all (`elem` schema) (lhs ++ rhs)

instance Show Relation where
    show (Rel s c) = show s ++ ": " ++ display c
        where
            wrap x = "(" ++ x ++ ")"
            display c = wrap (intercalate "|" (map show c))