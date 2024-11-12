module Data.Relations.Dependencies where
import Data.Relations ( Relation, Cover, FunctionalDependency (To), Attribute )

-- check whether a functional dependency is trivial
isTrivial :: FunctionalDependency -> Bool
isTrivial (To lhs rhs) = all (`elem` lhs) rhs

-- Compute the closure of a set of attributes within a given list
attrClosure :: Relation -> [Attribute] -> [Attribute]
attrClosure = undefined

-- check if an attribute set is a superkey
isSuperkey :: Relation -> [Attribute] -> Bool
isSuperkey = undefined

-- check if an attribute set is a key
isKey :: Relation -> [Attribute] -> Bool
isKey = undefined

-- get all keys of a relation
keysOf :: Relation -> [[Attribute]]
keysOf = undefined

-- Compute the closure of a set of FDs
fdClosure :: Relation -> Cover
fdClosure = undefined

-- check if a specific FD is implied by a cover
inClosure :: Cover -> FunctionalDependency -> Bool
inClosure = undefined

-- check if a set of FDs is in minimal form or not
isMinimal :: Cover -> Bool
isMinimal = undefined

-- Compute the minimal basis of an FD cover
-- minimize's output should always satisfy isBasis and isMinimal
minimize :: Cover -> Cover
minimize = undefined

