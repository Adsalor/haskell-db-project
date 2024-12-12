module Data.Relations.Normalization (is1NF, primeAttributes, is2NF, is3NF, isBCNF, dependencyIs2NF, dependencyIsBCNF)where 

import Data.Relations ( Relation (Rel), Attribute, FunctionalDependency (To))
import Data.Relations.Dependencies ( keysOf, isSuperkey, isTrivial, isKey )
import Data.Set qualified as S

-- Check if relation is in 1NF
-- this is a bit silly to add
-- but if a relation is expressible in our format
-- it has to be in 1NF
is1NF :: Relation -> Bool
is1NF = const True

-- Get prime attributes of a relation
primeAttributes :: Relation -> S.Set Attribute
primeAttributes r = S.unions (keysOf r)

-- Check if a dependency violates 2NF
dependencyIs2NF :: Relation -> FunctionalDependency -> Bool
dependencyIs2NF rel f@(l `To` r) = isTrivial f 
    || (let pa = primeAttributes rel in 
    (S.isSubsetOf r pa) -- Right is prime
    || all not (S.map (S.isProperSubsetOf l) (keysOf rel))) -- Right is not prime and fd is not partial

-- Check if relation is in 2NF
is2NF :: Relation -> Bool
is2NF r@(Rel s f) = let pa = primeAttributes r
                    in all (dependencyIs2NF r) f

-- Check if a dependency violates 3NF
dependencyIs3NF :: S.Set Attribute -> Relation -> FunctionalDependency -> Bool
dependencyIs3NF pa rel f@(l `To` r) = isTrivial f || isSuperkey rel l || S.isSubsetOf r pa

-- Check if relation is in 3NF
is3NF :: Relation -> Bool
is3NF r@(Rel s f) = let pa = primeAttributes r
                    in all (dependencyIs3NF pa r) f

-- Check if a dependency violates BCNF
dependencyIsBCNF :: Relation -> FunctionalDependency -> Bool
dependencyIsBCNF k f@(l `To` r) = isTrivial f || isSuperkey k l

-- Check if relation is in BCNF
isBCNF :: Relation -> Bool
isBCNF r@(Rel s f) = all (dependencyIsBCNF r) f