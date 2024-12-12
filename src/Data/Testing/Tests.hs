module Data.Testing.Tests where

import Data.Set qualified as S

import Data.Relations
import Data.Relations.Dependencies
    ( isTrivial, isSuperkey, isKey, keysOf )
import Data.Relations.Normalization ( is3NF, isBCNF )
import Data.Relations.Decomposition
    ( decompose3NF, decomposeBCNF, isLossless, isDependencyPreserving )
import Data.Relations.App ( fd, relation, extract )

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

s = snd $ extract relation "S(A,B,C,D,E,F,G) A->B,C A,C->D,E B->G C,D->E,F A,C,F->G A,F->E"
s1 = snd $ extract relation "S1(A,B,D,G)"
s2 = snd $ extract relation "S2(A,C,D,F)"
s3 = snd $ extract relation "S3(C,D,E)"
s4 = snd $ extract relation "S4(B,C,E,G)"

t = snd $ extract relation "R(A,B,C,D,E,F,G) A->B,G A,D->B,F B->C B,C->F,G A,C,G->E A,B->C,E D,E->F,G"
t1 = snd $ extract relation "R1(B,D,G)"
t2 = snd $ extract relation "R2(A,B,C,D,F)"
t3 = snd $ extract relation "R3(B,F)"
t4 = snd $ extract relation "R4(B,C,E,G)"

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

sDecompIsLossless = isLossless s [s1,s2,s3,s4]
tDecompIsLossy = not $ isLossless s [t1,t2,t3,t4]


-- [A,D,E,F]
r2Keys = keysOf r2



