module Data.Relations.Dependencies (
    toBasis,combineBasis,
    isTrivial,inSchema,
    isSuperkey,isKey,keysOf,
    attrClosure,fdClosure,inClosure,
    isMinimal,minimize) where

import Data.Relations ( Relation (Rel), Cover, FunctionalDependency (To), Attribute, Schema, leftSide, rightSide )
import Data.Set qualified as S
import Control.Monad.State qualified as ST
import Control.Monad (when, foldM, liftM2)
import Data.List (groupBy)

splitFD :: FunctionalDependency -> S.Set FunctionalDependency
splitFD (To lhs rhs) = S.map (To lhs . S.singleton) rhs

toBasis :: Cover -> Cover
toBasis fds = S.unions (S.map splitFD fds)

-- check whether a functional dependency is trivial
isTrivial :: FunctionalDependency -> Bool
isTrivial (To lhs rhs) = rhs `S.isSubsetOf` lhs

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
isKey rel attrs = isSuperkey rel attrs && not (any (isSuperkey rel . (`S.delete` attrs)) attrs)

recFindKeys :: Relation -> [S.Set Attribute] -> S.Set (S.Set Attribute) -> S.Set (S.Set Attribute)
recFindKeys _ [] s = s
recFindKeys rel@(Rel sch _) (el:fringe) keys
  | any (`S.isSubsetOf` el) keys = recFindKeys rel fringe keys
  | isSuperkey rel el = recFindKeys rel fringe (S.insert el keys)
  | otherwise = let unused = S.difference sch el
                    candidates = map (`S.insert` el) (S.toAscList unused)
                in recFindKeys rel (fringe ++ candidates) keys

-- get all keys of a relation
keysOf :: Relation -> S.Set (S.Set Attribute)
keysOf r@(Rel sch fds) =
    let requiredKeyAttrs = S.difference sch $ S.unions (S.map rightSide fds)
    in recFindKeys r [requiredKeyAttrs] S.empty

-- Compute the closure of a relation's FDs
-- note that this is provided in absolutely maximal form
-- and should not be used in practice unless strictly necessary
fdClosure :: Relation -> Cover
fdClosure r@(Rel sch fds) = S.map (\s -> To s (attrClosure r s)) (S.powerSet sch)

-- check if a specific FD is implied by a cover
inClosure :: Cover -> FunctionalDependency -> Bool
inClosure fds (To lhs rhs) = rhs `S.isSubsetOf` recAttrClosure fds lhs

-- Check if a specific FD is made of attributes of a schema
inSchema :: Schema -> FunctionalDependency -> Bool
inSchema s (To lhs rhs) = S.isSubsetOf lhs s && S.isSubsetOf rhs s

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

-- the state here is the new FDs
-- the first parameter and return value are the constant original FDs
-- using a state monad allows us to fold conveniently
remTrivialF :: Cover -> FunctionalDependency -> ST.State (S.Set FunctionalDependency) Cover
remTrivialF originals fd@(To lhs _) = do
    newFDs <- ST.get
    let lhsClosureOrig = recAttrClosure originals lhs
    let lhsClosureNew = recAttrClosure (S.delete fd newFDs) lhs
    when (lhsClosureOrig == lhsClosureNew) $ ST.modify (S.delete fd)
    return originals

recRemLHSF :: FunctionalDependency -> Cover ->  Cover
recRemLHSF fd@(To lhs rhs) cover = S.insert reducedFD (S.delete fd cover)
    where reducedFD = ST.evalState (foldM remLHF fd lhs) cover

-- state here is FDs pre-editing this FD
-- first param/return are FD before/after attribute evaluation
-- same logic for state monad
remLHF :: FunctionalDependency -> Attribute -> ST.State (S.Set FunctionalDependency) FunctionalDependency
remLHF fd@(To lhs rhs) attr = do
    orig <- ST.get
    let newLHS = S.delete attr lhs
    let withoutAttr = To newLHS rhs
    let new = S.insert withoutAttr (S.delete fd orig)
    let newLHSOrigClosure = recAttrClosure orig newLHS
    let newLHSNewClosure = recAttrClosure new newLHS
    if newLHSOrigClosure == newLHSNewClosure then return withoutAttr else return fd

-- Compute the minimal basis of an FD cover
-- minimize's output should always satisfy isBasis and isMinimal
minimize :: Cover -> Cover
minimize fds = let
    basis = toBasis fds
    purgeTrivial = ST.execState (foldM remTrivialF basis basis) basis -- basis basis basis basis
    purgeTrivialLHS = foldr recRemLHSF purgeTrivial purgeTrivial
    in purgeTrivialLHS

on :: (t1 -> t1 -> t2) -> (t3 -> t1) -> t3 -> t3 -> t2
on f g a b = f (g a) (g b)

combineBasis :: Cover -> Cover
combineBasis = S.fromAscList . map (liftM2 To (leftSide . head) (S.unions . map rightSide)) . groupBy ((==) `on` leftSide) . S.toAscList