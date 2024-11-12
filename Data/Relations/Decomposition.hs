module Data.Relations.Decomposition where
import Data.Relations ( Relation, Attribute, Cover, Schema )

-- Decompose a relation to a list of relations following
-- second normal form
decompose2NF :: Relation -> [Relation]
decompose2NF = undefined

-- Decompose a relation to a list of relations following
-- third normal form
decompose3NF :: Relation -> [Relation]
decompose3NF = undefined

-- Decompose a relation to a list of relations following
-- Boyce-Codd normal form
decomposeBCNF :: Relation -> [Relation]
decomposeBCNF = undefined

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
projectDependencies :: Cover -> Schema -> Relation
projectDependencies = undefined


