module Data.Relations.Dependencies where

import Data.Relations ( Relation (Rel), Cover, FunctionalDependency (To), Attribute, leftSide, rightSide )
import Data.Set qualified as S

splitFD :: FunctionalDependency -> S.Set FunctionalDependency
splitFD (To lhs rhs) = S.map (To lhs . S.singleton) rhs

toBasis :: Cover -> Cover
toBasis fds = S.unions (S.map splitFD fds)

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
isKey rel attrs = isSuperkey rel attrs && not (any (isSuperkey rel . flip S.delete attrs) attrs)

-- get all keys of a relation
keysOf :: Relation -> S.Set (S.Set Attribute)
keysOf (Rel sch fds) = 
    let requiredKeyAttrs = S.difference sch (S.unions (S.map rightSide fds))  
    in undefined

-- Compute the closure of a set of FDs
fdClosure :: Relation -> Cover
fdClosure r@(Rel sch fds) = S.map (\s -> To s (attrClosure r s)) (S.powerSet sch)

-- check if a specific FD is implied by a cover
inClosure :: Cover -> FunctionalDependency -> Bool
inClosure fds (To lhs rhs) = rhs `S.isSubsetOf` recAttrClosure fds lhs

-- check if a set of FDs is in minimal form or not
isMinimal :: Cover -> Bool
isMinimal fds = let
    basis = toBasis fds
    allNeeded = not $ any (\fd -> inClosure (S.delete fd basis) fd) basis
    allLHSNeeded = all (lhsNeeded basis) basis
        where
            lhsNeeded :: Cover -> FunctionalDependency -> Bool
            lhsNeeded fds (To lhs rhs) = not $ any (\a -> inClosure fds (To (S.delete a lhs) rhs)) lhs
    in allNeeded && allLHSNeeded

-- Compute the minimal basis of an FD cover
-- minimize's output should always satisfy isBasis and isMinimal
minimize :: Cover -> Cover
minimize fds = let
    basis = toBasis fds
    purgeTrivial = undefined
    purgeTrivialLHS = undefined
    in purgeTrivialLHS
