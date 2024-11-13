module Data.Testing.Tests where
import Data.Relations.Decomposition

r1 = Rel (map Attr ["A", "B", "C", "D", "E", "F"]) [[Attr "A"] `To` map Attr ["B", "C"], [Attr "D"] `To` map Attr ["E", "F"]]
r2 = Rel (map Attr ["A", "B", "C", "D", "E", "F"]) [map Attr ["A", "B"] `To` [Attr "C"], [Attr "F"] `To` [Attr "B"]]
r3 = Rel (map Attr ["A", "B", "C"]) [[Attr "A"] `To` [Attr "B"], [Attr "B"] `To` [Attr "A"]]

-- R1(A,B,C) F1 = {A -> BC}
-- R2(D,E,F) F2 = {D -> EF}
-- R3(A,D) F3 = {}
decompose3NF r1


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
is3NF r3 == true
isBCNF r3 == false

-- [A,D,E,F]
keysOf r2



