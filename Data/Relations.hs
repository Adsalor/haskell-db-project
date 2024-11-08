module Data.Relations where

import Data.Set ( Set, union, isSubsetOf, toAscList )
import Data.List (intercalate)

newtype Attribute = Attr { str :: String } deriving (Ord, Eq)
instance Show Attribute where
    show = str

type Schema = Set Attribute

data FunctionalDependency = To (Set Attribute) (Set Attribute)
instance Show FunctionalDependency where
    show (To l r) = display l ++ "->" ++ display r
        where
            display :: Set Attribute -> String
            display s = intercalate "," (map str $ toAscList s)

type Basis = Set FunctionalDependency

data Relation = Rel Schema Basis

verify :: Relation -> Bool
verify (Rel schema fds) = all (check schema) fds
    where
        check :: Schema -> FunctionalDependency -> Bool
        check schema (To lhs rhs) = (lhs `union` rhs) `isSubsetOf` schema