module Data.Relations.App where
import Data.Relations
import Data.Relations.Dependencies
import Data.Relations.Decomposition
import Data.Relations.Normalization
import Text.Parsec
import Text.Parsec.String

-- Parsing

name :: Parser (String,Schema)
name = undefined

fd :: Parser FunctionalDependency
fd = undefined

relation :: Parser (String,Relation)
relation = do
    (id,sch) <- name
    fds <- many fd
    return (id,Rel sch fds)

fileParser :: Parser [(String,Relation)]
fileParser = do
    x <- many1 relation 
    eof
    return x

-- for the actual app: we intend to make a state monad to pass named relation environment
-- so we can go for a REPL-style loop
-- haven't done this yet because not 100% sure how to combine IO monad and that state monad
-- so no solid signatures
-- but will be something like

-- load in files from input
-- readFile :: IO (Maybe [(String,Relation)]) -- or some other type depending on the working of the monad

-- prompt the user to take a given action from the list (view relation, select relations, perform action)
-- mainMenu :: IO ()

-- list all relations in the environment to the user
-- listRelations :: _ () -- not just IO

-- rename :: String -> String -> _ ()

-- relationMenu :: _ ()

-- getClosure :: _ ()

-- getKeys :: _ ()

-- getFDClosure :: _ ()

-- minimalBasis :: _ ()

-- decomposeMenu :: _ ()

-- selectRelations :: _ [Relation]

-- 