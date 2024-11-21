module Data.Testing.Tests where

import Data.Set qualified as S

import Data.Relations
import Data.Relations.Dependencies
import Data.Relations.Normalization
import Data.Relations.Decomposition
import Data.Relations.App

r1parsed = snd $ extract relation "R1(A,B,C,D,E,F) A->B,C D->E,F"
r2parsed = snd $ extract relation "R2(A,B,C,D,E,F) A,B->C F->B"
r3parsed = snd $ extract relation "R3(A,B,C) A->B B->A"

r1 = Rel (S.fromAscList $ map Attr ["A", "B", "C", "D", "E", "F"]) $
    S.insert (S.singleton (Attr "A") `To` S.fromAscList (map Attr ["B", "C"])) $
    S.insert (S.singleton (Attr "D") `To` S.fromAscList (map Attr ["E", "F"])) S.empty
r2 = Rel (S.fromAscList $ map Attr ["A", "B", "C", "D", "E", "F"]) $
    S.insert (S.fromAscList (map Attr ["A", "B"]) `To` S.singleton (Attr "C")) $
    S.insert (S.singleton (Attr "F") `To` S.singleton (Attr "B")) S.empty
r3 = Rel (S.fromAscList $ map Attr ["A", "B", "C"]) $
    S.insert (S.singleton (Attr "A") `To` S.singleton (Attr "B")) $
    S.insert (S.singleton (Attr "B") `To` S.singleton (Attr "A")) S.empty

-- R1(A,B,C) F1 = {A -> BC}
-- R2(D,E,F) F2 = {D -> EF}
-- R3(A,D) F3 = {}
r1in3NF = decompose3NF r1


-- R1(A,B,C) F1 = {AB ->C}
-- R21(F,B) F21 = {F->B}
-- R22(A,D,E,F) F22 = {} 
r2inBCNF = decomposeBCNF r2


r1lossless3nf = isLossless r1 (decompose3NF r1)
r1depPres3nf = isDependencyPreserving r1 (decompose3NF r1)
abcAtrivial = isTrivial $ extract fd "A,B,C->A"
r1HasKeyAD = isKey r1 (S.fromAscList $ map Attr ["A", "D"])
r1HasSuperkeyABD = isSuperkey r1 (S.fromAscList $ map Attr ["A", "B", "D"])
r13nf3nf = all is3NF (decompose3NF r1)
r2bcnfBcnf = all isBCNF (decomposeBCNF r2)
r3is3NF = is3NF r3
r3isBCNF = isBCNF r3

-- [A,D,E,F]
r2Keys = keysOf r2



