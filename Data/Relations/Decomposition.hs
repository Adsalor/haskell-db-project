module Data.Relations.Decomposition (decompose2NF, decompose3NF, decomposeBCNF, projectDependencies, isLossless, isDependencyPreserving) where
import Data.List
import Data.Set qualified as S
import Data.Relations ( Relation (Rel), Attribute (Attr), Cover, Schema, leftSide, rightSide, FunctionalDependency (To))
import Data.Relations.Dependencies ( fdClosure, inSchema, minimize, keysOf, attrClosure, toBasis, minimize, combineBasis )
import Data.Relations.Normalization ( is3NF, isBCNF, dependencyIsBCNF, dependencyIs2NF )

-- Given a dependency that violates 2NF, create 2 relations based on that dependency that follow 2NF
splitDependencyInto2NF :: Relation -> FunctionalDependency  -> [Relation]
splitDependencyInto2NF rel@(Rel s f) fd@(l `To` r)  = let leftClose = attrClosure rel l
    in [projectDependencies f (S.union (S.difference s leftClose) l), projectDependencies f leftClose]


-- Decompose a relation to a list of relations following
-- second normal form
decompose2NF :: Relation -> [Relation]
decompose2NF rel@(Rel s f) = let
    notIn2NF = S.filter (not . dependencyIs2NF rel) f
    in2NF = S.difference f notIn2NF
    closures = S.map (\f@(l `To` r) -> attrClosure rel l) notIn2NF
    leftSides = S.unions (S.map (\f@(l `To` r) -> l) notIn2NF)
    notInDependency = S.difference s (S.unions (S.map (\f@(l `To` r) -> S.union l r) f))
    notInFDAndLeftSides = S.union notInDependency leftSides
    in projectDependencies f notInFDAndLeftSides : map (projectDependencies f) (S.toList closures)

-- Create a relation where the schema are the attributes in a functional dependency
-- And the dependencies are projected from an original relation
fdToRelation :: Relation -> FunctionalDependency -> Relation
fdToRelation r@(Rel sch fds) fd@(To lhs rhs) =
    let s = S.union lhs rhs
    in projectDependencies fds s

-- Add a relation containing the attributes of the key to the 3NF decomp, if necessary
addKey :: Relation -> [Relation] -> [Relation]
addKey r@(Rel s f) rs
    -- If key is already in a relation
    | any (S.isSubsetOf (S.findMin (keysOf r))) (S.fromList (map (\r@(Rel sch fds) -> sch) rs)) = rs
    | otherwise = projectDependencies f (S.findMin (keysOf r)) : rs

-- Remove relations in the decomposition that are a subset of a specific relation
removeSubsetsOfRelation :: [Relation] -> Relation -> [Relation]
removeSubsetsOfRelation rs r@(Rel s f)  = filter (\rel@(Rel s1 f1) -> not (S.isProperSubsetOf s1 s)) rs

-- Remove all relations in the decomposition that are subsets of other relations
removeSubsets :: [Relation] -> [Relation]
removeSubsets rs = let
    removed = map (removeSubsetsOfRelation rs) rs
    smallest = minimum (map length removed)
    largest = maximum (map length removed)
    in
    if smallest < largest then removeSubsets (head (filter ((== smallest) . length) removed))
    else head removed

-- Decompose a relation to a list of relations following
-- third normal form
decompose3NF :: Relation -> [Relation]
decompose3NF r@(Rel sch fds)
    | is3NF r = [r]
    | otherwise = let minBasis = minimize fds in
        removeSubsets (addKey r (map (fdToRelation r) (S.toList fds)))

-- Perform a step of the BCNF decomp on a given FD X->Y
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

-- Given the schema of a relation and the schema of a decomposition component
-- Construct a row of the canonical basis for chase decomposition
constructRow :: [Attribute] -> [Attribute] -> Int -> [Attribute]
constructRow [] s i = []
constructRow (a:as) s i =
    if a `elem` s then Attr (show a) : constructRow as s i
    else Attr (show a ++ show i) : constructRow as s i

-- Modify a row of the canonical basis to remove subscripts for certain elements, 
-- specified by an array of booleans
changeRow :: Bool -> [Bool] -> [Attribute] -> [Attribute] -> [Attribute]
changeRow False _ names _ = names
changeRow _ [] _ _ = []
changeRow _ bools@(b:bs) cbNames@(newA:newAs) relationNames@(oldA:oldAs)
    = if b then oldA : changeRow True bs newAs oldAs
    else newA : changeRow True bs newAs oldAs

-- Create the canonical basis for chase decomposition
canonicalBasis :: Relation -> [Relation] -> Int -> [[Attribute]]
canonicalBasis r [] _ = []
canonicalBasis r@(Rel s f) (r2@(Rel s2 f2):rs) i =
    constructRow (S.toList s) (S.toList s2) i : canonicalBasis r rs (i+1)

-- Given the schema of a relation and a functonal dependency,
-- make a boolean list the length of the schema where every entry is
-- true if the attribute is in the RHS of the FD and false otherwise
getAttributesOnRight :: FunctionalDependency -> [Attribute] -> [Bool]
getAttributesOnRight _ [] = []
getAttributesOnRight fd@(l `To` r) (a:as) = S.member a r : getAttributesOnRight fd as

-- Modify all rows, removing subscripts from attributes in rows that follow a functional dependency
changeRows :: [Bool] -> [[Attribute]] -> Relation -> FunctionalDependency -> [[Attribute]]
changeRows [] _ _ _ = []
changeRows left@(b:bs) (cb:cbs) r@(Rel s f) fd =
    if length (filter id left) > 1
        then changeRow b (getAttributesOnRight fd (S.toList s)) cb (S.toList s) : changeRows bs cbs r fd
    else cb:cbs

-- Perform the chase algorithm to determine if a decomposition is lossless
chase :: [[Attribute]] -> [FunctionalDependency] -> Relation -> [FunctionalDependency] -> [[Attribute]]
chase cb [] _ _ = cb
chase cb (f@(l `To` r):fs) rel allFds =
    let
    left = map (S.isSubsetOf l . S.fromList) cb -- Boolean list of if each row has the lefthand values of the FD subscriptless
    right = map (S.isSubsetOf r . S.fromList) cb
    both = zipWith (&&) left right
    new = changeRows left cb rel f
    in
    if new == cb then chase cb fs rel allFds
    else chase new allFds rel allFds

-- Return true if there is a row with no subscripts
allSubscriptless :: [[Attribute]] -> [Attribute] -> Bool
allSubscriptless cb s = s `elem` cb

-- Takes an original relation and a decomposition of the relation and
-- checks if the decomposition is lossless
isLossless :: Relation -> [Relation] -> Bool
isLossless rel@(Rel s f) rels = let minF = toBasis f in allSubscriptless (chase (canonicalBasis rel rels 1) (S.toList minF) rel (S.toList minF)) (S.toList s)

-- Takes an original relation and a decomposition of the relation and
-- checks if the decomposition is dependency preserving
isDependencyPreserving :: Relation -> [Relation] -> Bool
isDependencyPreserving rel@(Rel s f) rels =
    minimize f == minimize (S.unions (S.fromList (map (\r@(Rel s2 f2) -> f2) rels)))

-- Given an original cover and a schema, creates a new relation containing
-- the schema and the projected functional dependencies from the cover
-- onto that schema
projectDependencies :: Cover -> Schema -> Relation
projectDependencies c sch = Rel sch (combineBasis $ minimize (S.filter (inSchema sch) (toBasis (fdClosure (Rel sch c)))))

-- Project FDs from a relation onto a set of schemas.
projectRelation :: Relation -> [Schema] -> [Relation]
projectRelation r@(Rel sch fds) = map (projectDependencies fds)