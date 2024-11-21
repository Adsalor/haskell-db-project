module Data.Relations.Normalization where 

import Data.Relations ( Relation (Rel), Attribute, FunctionalDependency (To))
import Data.Relations.Dependencies ( keysOf )
import Data.Set qualified as S

is1NF :: Relation -> Bool
is1NF = const True

primeAttributes :: Relation -> S.Set Attribute
primeAttributes r = S.unions (keysOf r)

is2NF :: Relation -> Bool
is2NF = undefined

dependencyIs3NF :: S.Set Attribute -> S.Set (S.Set Attribute) -> FunctionalDependency -> Bool
dependencyIs3NF pa k (l `To` r) = S.member l k || S.isSubsetOf r pa

is3NF :: Relation -> Bool
is3NF r@(Rel s f) = let pa = primeAttributes r;
                        k = keysOf r 
                        in all (dependencyIs3NF pa k) f

dependencyIsBCNF :: S.Set (S.Set Attribute) -> FunctionalDependency -> Bool
dependencyIsBCNF k (l `To` r) = S.member l k

isBCNF :: Relation -> Bool
isBCNF r@(Rel s f) = let k = keysOf r in all (dependencyIsBCNF k) f