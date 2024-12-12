module Data.Relations.Dependencies (
    toBasis, combineBasis,
    isTrivial, inSchema,
    isSuperkey, isKey, keysOf,
    attrClosure, fdClosure, inClosure,
    applyFD,
    isMinimal, minimize ) where

import Data.Relations ( Relation (Rel), Cover, FunctionalDependency (To), Attribute, Schema, leftSide, rightSide, attributesOf )
import Data.Set qualified as S
import Control.Monad.State qualified as ST
import Control.Monad ( when, foldM, liftM2, ap )
import Data.List ( groupBy )
import Data.Function ( on )

-- splits an FD using the splitting rule into a set of FDs from the left side to each right side attribute
splitFD :: FunctionalDependency -> S.Set FunctionalDependency
splitFD (To lhs rhs) = S.map (To lhs . S.singleton) rhs

-- converts any given cover to a basis by ensuring right sides of each FD only have one element
-- output must satisfy isBasis
toBasis :: Cover -> Cover
toBasis fds = S.unions (S.map splitFD fds)

-- recombine the right sides of a basis into a condensed cover
-- i.e. turn (A->B | A->C) into (A->BC)
-- toBasis and combineBasis are idempotent on themselves and each other
-- (toBasis . combineBasis) == toBasis
-- (combineBasis . toBasis) == combineBasis
combineBasis :: Cover -> Cover
combineBasis = S.fromAscList . map (liftM2 To (leftSide . head) (S.unions . map rightSide)) . groupBy ((==) `on` leftSide) . S.toAscList

-- check whether a functional dependency is trivial
isTrivial :: FunctionalDependency -> Bool
isTrivial (To lhs rhs) = rhs `S.isSubsetOf` lhs

-- checks whether a functional dependency can be applied on a set of attributes
canApply :: S.Set Attribute -> FunctionalDependency -> Bool
canApply attrs (To lhs _) = lhs `S.isSubsetOf` attrs

-- attempts to apply a single FD to a set of attributes
applyFD :: FunctionalDependency -> S.Set Attribute -> S.Set Attribute
applyFD fd@(To lhs rhs) attrs = if attrs `canApply` fd then S.union rhs attrs else rhs

-- recursively computes attribute closure in a set of FDs by applying all applicable FDs
recAttrClosure :: S.Set FunctionalDependency -> S.Set Attribute -> S.Set Attribute
recAttrClosure fds attrs = let (applicable,nonapplicable) = S.partition (canApply attrs) fds in
    if null applicable then attrs else recAttrClosure nonapplicable (foldr applyFD attrs applicable)

-- Compute the closure of a set of attributes within a given relation
attrClosure :: Relation -> S.Set Attribute -> S.Set Attribute
attrClosure (Rel sch fds) = recAttrClosure fds

-- check if an attribute set is a superkey of a relation
isSuperkey :: Relation -> S.Set Attribute -> Bool
isSuperkey rel@(Rel sch fds) attrs = attrClosure rel attrs == sch

-- check if an attribute set is a key of a relation
isKey :: Relation -> S.Set Attribute -> Bool
isKey rel attrs = isSuperkey rel attrs && not (any (isSuperkey rel . (`S.delete` attrs)) attrs)

-- recursively find keys by exploring a fringe space of possible keys
-- if a possibility is not a key, then it is augmented with every attribute
-- and added to the fringe
-- the key insight here is that the fringe is in 'increasing' order, so it is 
-- impossible to check a superset before a subset and false-flag a key
-- as far as I can tell, this is an original algorithm
-- it's not in any of the papers or textbooks I checked
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
fdClosure r@(Rel sch fds) = S.map (ap To (attrClosure r)) (S.powerSet sch)

-- check if a specific FD is implied by a cover
inClosure :: Cover -> FunctionalDependency -> Bool
inClosure fds (To lhs rhs) = rhs `S.isSubsetOf` recAttrClosure fds lhs

-- Check if a specific FD is made of attributes of a schema
inSchema :: Schema -> FunctionalDependency -> Bool
inSchema s fd = attributesOf fd `S.isSubsetOf` s

-- check if a set of FDs is in minimal form or not
isMinimal :: Cover -> Bool
isMinimal fds = let
    basis = toBasis fds
    allNeeded = not $ any ((`S.delete` basis) >>= inClosure) basis
    allLHSNeeded = all (lhsNeeded basis) basis
        where
            lhsNeeded :: Cover -> FunctionalDependency -> Bool
            lhsNeeded fds (To lhs rhs) = not $ any (inClosure fds . (`To` rhs) . (`S.delete` lhs)) lhs
    in allNeeded && allLHSNeeded

-- the state here is the new FDs
-- the first parameter and return value are the constant original FDs
-- using a state monad allows us to fold conveniently
-- folding function to remove trivial FDs from a relation
remTrivialF :: Cover -> FunctionalDependency -> ST.State (S.Set FunctionalDependency) Cover
remTrivialF originals fd@(To lhs _) = do
    newFDs <- ST.get
    when (((==) `on` (`recAttrClosure` lhs)) originals (S.delete fd newFDs)) (ST.modify (S.delete fd))
    return originals

-- remove left hand side attributes from FDs in a fold
-- basically just replaces the old with the new
remLHSF :: FunctionalDependency -> Cover ->  Cover
remLHSF fd@(To lhs rhs) cover = S.insert reducedFD (S.delete fd cover)
    where reducedFD = ST.evalState (foldM remLHF fd lhs) cover

-- state here is FDs pre-editing this FD
-- first param/return are FD before/after attribute evaluation
-- same logic for state monad
-- folding function to attempt removing trivial LHS attributes from an FD
remLHF :: FunctionalDependency -> Attribute -> ST.State (S.Set FunctionalDependency) FunctionalDependency
remLHF fd@(To lhs rhs) attr = do
    orig <- ST.get
    let newLHS = S.delete attr lhs
    let withoutAttr = To newLHS rhs
    let new = S.insert withoutAttr (S.delete fd orig)
    return $ if ((==) `on` (`recAttrClosure` newLHS)) orig new then withoutAttr else fd

-- Compute the minimal basis of an FD cover
-- minimize's output should always satisfy isBasis and isMinimal
minimize :: Cover -> Cover
minimize fds = let
    basis = toBasis fds
    purgeTrivial = ST.execState (foldM remTrivialF basis basis) basis -- basis basis basis basis
    purgeTrivialLHS = foldr remLHSF purgeTrivial purgeTrivial
    in purgeTrivialLHS
