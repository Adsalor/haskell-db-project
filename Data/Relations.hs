module Data.Relations where

import Data.Set

newtype Attribute = Attr { str :: String }

type Schema = Set Attribute

data FunctionalDependency = To (Set Attribute) (Set Attribute)

type Basis = Set FunctionalDependency

data Relation = Rel Schema Basis
