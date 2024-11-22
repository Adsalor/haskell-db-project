module Data.Relations.Decomposition where
import Data.Set qualified as S
import Data.Relations ( Relation (Rel), Attribute, Cover, Schema, leftSide, rightSide, FunctionalDependency (To))
import Data.Relations.Dependencies ( fdClosure, lhsClosure, inSchema, minimize, keysOf, attrClosure, toBasis )
import Data.Relations.Normalization ( is3NF, isBCNF, dependencyIsBCNF )
-- Decompose a relation to a list of relations following
-- second normal form
decompose2NF :: Relation -> [Relation]
decompose2NF = undefined

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
decomposeBCNF r@(Rel sch fds) 
    | isBCNF r = [r]
    | otherwise = 
        let violating = S.findMin (S.filter (not . dependencyIsBCNF r) fds)
        in concatMap decomposeBCNF (decomposeOn r violating)

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
projectDependencies c sch = Rel sch (S.filter (inSchema sch) (toBasis (fdClosure (Rel sch c))))

-- Project FDs from a relation onto a set of schemas.
projectRelation :: Relation -> [Schema] -> [Relation]
projectRelation r@(Rel sch fds) = map (projectDependencies fds)