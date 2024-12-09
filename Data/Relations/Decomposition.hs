module Data.Relations.Decomposition where
import Data.Set qualified as S
import Data.Relations ( Relation (Rel), Attribute, Cover, Schema, leftSide, rightSide, FunctionalDependency (To))
import Data.Relations.Dependencies ( fdClosure, inSchema, minimize, keysOf, attrClosure, toBasis, minimize )
import Data.Relations.Normalization ( is3NF, isBCNF, dependencyIsBCNF, dependencyIs2NF )


-- Get all supersets of a subset (as in, given set (A,B,C) and attribute A, return ((A), (A,B), (A,C), (A,B,C)))
lhsPowerSet :: S.Set FunctionalDependency -> Schema -> S.Set (S.Set Attribute)
lhsPowerSet = undefined

-- Compute the closure of all LHS' of FDs
lhsClosure :: Relation -> Cover
lhsClosure r@(Rel sch fds) = S.map (\s -> To s (attrClosure r s)) (lhsPowerSet fds sch)


splitDependencyInto2NF :: Relation -> FunctionalDependency  -> [Relation]
splitDependencyInto2NF rel@(Rel s f) fd@(l `To` r)  = let leftClose = attrClosure rel l
    in [projectDependencies f (S.union (S.difference s leftClose) l), projectDependencies f leftClose]


-- Decompose a relation to a list of relations following
-- second normal form
decompose2NF :: Relation -> [Relation]
decompose2NF rel@(Rel s f) = let 
    notIn2NF = S.filter (\fd -> not (dependencyIs2NF rel fd)) f
    in2NF = S.difference f notIn2NF
    closures = S.map (\f@(l `To` r) -> attrClosure rel l) notIn2NF
    leftSides = S.unions (S.map (\f@(l `To` r) -> l) notIn2NF)
    notInDependency = S.difference s (S.unions (S.map (\f@(l `To` r) -> S.union l r) f))
    notInFDAndLeftSides = S.union notInDependency leftSides
    in projectDependencies f notInFDAndLeftSides : (map (projectDependencies f) (S.toList closures))


fdToRelation :: Relation -> FunctionalDependency -> Relation
fdToRelation r@(Rel sch fds) fd@(To lhs rhs) = 
    let s = S.union lhs rhs
    in projectDependencies fds s

addKey :: Relation -> [Relation] -> [Relation]
addKey r@(Rel s f) rs 
    -- If key is already in a relation
    | any (S.isSubsetOf (S.findMin (keysOf r))) (S.fromList (map (\r@(Rel sch fds) -> sch) rs)) = rs
    | otherwise = projectDependencies f (S.findMin (keysOf r)) : rs


-- Decompose a relation to a list of relations following
-- third normal form
-- *** Need to implement removing relations that are subsets of other relations
decompose3NF :: Relation -> [Relation]
decompose3NF r@(Rel sch fds) 
    | is3NF r = [r]
    | otherwise = let minBasis = minimize fds in
        addKey r (map (fdToRelation r) (S.toList fds))


decomposeOn :: Relation -> FunctionalDependency -> [Relation]
decomposeOn r@(Rel sch fds) fd@(To lhs rhs) = 
    let lhsClosure = attrClosure r lhs
        r1 = projectDependencies fds lhsClosure
        r2 = projectDependencies fds (S.difference sch (S.difference lhsClosure lhs)) in
        [r1, r2]

-- Decompose a relation to a list of relations following
-- Boyce-Codd normal form
decomposeBCNF :: Relation -> [Relation]
decomposeBCNF r@(Rel sch fds) =
    let violating = S.lookupMin (S.filter (not . dependencyIsBCNF r) fds)
    in maybe [r] (concatMap decomposeBCNF . decomposeOn r) violating

-- Takes an original relation and a decomposition of the relation and
-- checks if the decomposition is lossless
isLossless :: Relation -> [Relation] -> Bool
isLossless = undefined

-- Takes an original relation and a decomposition of the relation and
-- checks if the decomposition is dependency preserving
isDependencyPreserving :: Relation -> [Relation] -> Bool
isDependencyPreserving = undefined

-- Given an original cover and a schema, creates a new relation containing
-- the schema and the projected functional dependencies from the cover
-- onto that schema
-- *** Currently uses fdClosure, change to lhsClosure once that is implemented
projectDependencies :: Cover -> Schema -> Relation
projectDependencies c sch = Rel sch (minimize (S.filter (inSchema sch) (toBasis (fdClosure (Rel sch c)))))

-- Project FDs from a relation onto a set of schemas.
projectRelation :: Relation -> [Schema] -> [Relation]
projectRelation r@(Rel sch fds) = map (projectDependencies fds)