module Data.Testing.Tests where
import Data.Relations.Decomposition

r1 = Rel ["A", "B", "C", "D", "E", "F"] [["A"] `To` ["B", "C"], ["D"] `To` ["E", "F"]]
r2 = Rel ["A", "B", "C", "D", "E", "F"] [["A", "B"] `To` ["C"], ["F"] `To` ["B"]]


-- R1(A,B,C) F1 = {A -> BC}
-- R2(D,E,F) F2 = {D -> EF}
-- R3(A,D) F3 = {}
decompose3NF r1


-- Either 
-- R1(B,F) F1 = {F -> B}
-- R2(A,C,D,E,F) F2 = {}
-- or
-- R1(A,B,C) F1 = {AB ->C}
-- R21(F,B) F21 = {F->B}
-- R22(A,D,E,F) F22 = {} 

decomposeBCNF r2



isLossless (decompose3NF r1)== true
isDependencyPreserving (decompose3NF r1) == true
isTrivial ["A", "B", "C"] `To` ["A"] == true
isKey r1 ["A", "D"] == true
isSuperKey r1 ["A", "B", "D"] == true
is3NF (decompose3NF r1) == true
isBCNF (decomposeBCNF r2) == true

-- [A,D,E,F]
keysOf r2



