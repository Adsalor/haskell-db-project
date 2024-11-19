module Data.Relations.Dependencies where

import Data.Relations ( Relation (Rel), Cover, FunctionalDependency (To), Attribute )
import Data.Set qualified as S

-- check whether a functional dependency is trivial
isTrivial :: FunctionalDependency -> Bool
isTrivial (To lhs rhs) = lhs `S.isSubsetOf` rhs

canApply :: S.Set Attribute -> FunctionalDependency -> Bool
canApply attrs (To lhs _) = lhs `S.isSubsetOf` attrs

-- attempt to apply a single FD to a set of attributes
applyFD :: FunctionalDependency -> S.Set Attribute -> S.Set Attribute
applyFD fd@(To lhs rhs) attrs = if attrs `canApply` fd then S.union rhs attrs else rhs

recAttrClosure :: S.Set FunctionalDependency -> S.Set Attribute -> S.Set Attribute
recAttrClosure fds attrs = let (applicable,nonapplicable) = S.partition (canApply attrs) fds in
    if null applicable then attrs else recAttrClosure nonapplicable (foldr applyFD attrs applicable)

-- Compute the closure of a set of attributes within a given list
attrClosure :: Relation -> S.Set Attribute -> S.Set Attribute
attrClosure (Rel sch fds) = recAttrClosure fds

-- check if an attribute set is a superkey
isSuperkey :: Relation -> S.Set Attribute -> Bool
isSuperkey rel@(Rel sch fds) attrs = attrClosure rel attrs == sch

-- check if an attribute set is a key
isKey :: Relation -> S.Set Attribute -> Bool
isKey = undefined

-- get all keys of a relation
keysOf :: Relation -> [S.Set Attribute]
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

