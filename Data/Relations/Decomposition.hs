module Data.Relations.Decomposition where
import Data.List 
import Data.Set qualified as S
import Data.Relations ( Relation (Rel), Attribute (Attr), Cover, Schema, leftSide, rightSide, FunctionalDependency (To))
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

removeSubsetsOfRelation :: [Relation] -> Relation -> [Relation]
removeSubsetsOfRelation rs r@(Rel s f)  = filter (\rel@(Rel s1 f1) -> not (S.isProperSubsetOf s1 s)) (rs)


removeSubsets :: [Relation] -> [Relation]
removeSubsets rs = let 
    removed = (map (removeSubsetsOfRelation rs) rs) 
    smallest = minimum (map length removed)
    largest = maximum (map length removed)
    in
    if smallest < largest then removeSubsets (head (filter (\r -> (length r == smallest)) removed))
    else (head removed)

-- Decompose a relation to a list of relations following
-- third normal form
-- *** Need to implement removing relations that are subsets of other relations
decompose3NF :: Relation -> [Relation]
decompose3NF r@(Rel sch fds) 
    | is3NF r = [r]
    | otherwise = let minBasis = minimize fds in
        removeSubsets (addKey r (map (fdToRelation r) (S.toList fds)))


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

constructRow :: [Attribute] -> [Attribute] -> Int -> [Attribute]
constructRow [] s i = []
constructRow (a:as) s i = 
    if a `elem` s then Attr (show a) : constructRow as s i
    else Attr (show a ++ show i) : constructRow as s i

changeRow :: Bool -> [Bool] -> [Attribute] -> [Attribute] -> [Attribute]
changeRow False _ names _ = names
changeRow _ [] _ _ = []
changeRow _ bools@(b:bs) cbNames@(newA:newAs) relationNames@(oldA:oldAs)  
    = if b then newA : changeRow True bs newAs oldAs
    else oldA : changeRow True bs newAs oldAs

canonicalBasis :: Relation -> [Relation] -> Int -> [[Attribute]]
canonicalBasis r [] _ = []
canonicalBasis r@(Rel s f) (r2@(Rel s2 f2):rs) i = 
    constructRow (S.toList s) (S.toList s2) i : canonicalBasis r rs (i+1)

getAttributesOnLeft :: FunctionalDependency -> [Attribute] -> [Bool]
getAttributesOnLeft _ [] = []
getAttributesOnLeft fd@(l `To` r) (a:as) = S.member a l : getAttributesOnLeft fd as

changeRows :: [Bool] -> [Bool] -> [Bool] -> [[Attribute]] -> Relation -> FunctionalDependency -> [[Attribute]]
changeRows [] _ _ _ _ _ = []
changeRows left@(b:bs) right both (cb:cbs) r@(Rel s f) fd =
    if length (filter (== True) both) > 1
        then (changeRow b (getAttributesOnLeft fd (S.toList s)) cb (S.toList s)) : changeRows bs right both cbs r fd
    else cb:cbs


chase :: [[Attribute]] -> [FunctionalDependency] -> Relation -> [FunctionalDependency] -> [[Attribute]]
chase cb [] _ _ = cb
chase cb ((f@(l `To` r)):fs) rel allFds = 
    let 
    left = map (S.isSubsetOf l . S.fromList) cb -- Boolean list of if each row has the lefthand values of the FD subscriptless
    right = map (S.isSubsetOf r . S.fromList) cb
    both = zipWith (&&) left right
    new = changeRows left right both cb rel f
    in
    if new == cb then chase cb fs rel allFds
    else new --chase new allFds rel allFds


-- Takes an original relation and a decomposition of the relation and
-- checks if the decomposition is lossless
isLossless :: Relation -> [Relation] -> [[Attribute]]
isLossless rel@(Rel s f) rels = let minF = toBasis f in chase (canonicalBasis rel rels 1) (S.toList minF) rel (S.toList minF)

-- Takes an original relation and a decomposition of the relation and
-- checks if the decomposition is dependency preserving
isDependencyPreserving :: Relation -> [Relation] -> Bool
isDependencyPreserving rel@(Rel s f) rels = 
    minimize f == minimize (S.unions (S.fromList (map (\r@(Rel s2 f2) -> f2) rels)))

-- Given an original cover and a schema, creates a new relation containing
-- the schema and the projected functional dependencies from the cover
-- onto that schema
-- *** Currently uses fdClosure, change to lhsClosure once that is implemented
projectDependencies :: Cover -> Schema -> Relation
projectDependencies c sch = Rel sch (minimize (S.filter (inSchema sch) (toBasis (fdClosure (Rel sch c)))))

-- Project FDs from a relation onto a set of schemas.
projectRelation :: Relation -> [Schema] -> [Relation]
projectRelation r@(Rel sch fds) = map (projectDependencies fds)