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

-- rename a specific environment to a new name
-- rename :: String -> String -> _ ()

-- provide the list of actions which can be taken on a specific relation
-- likely (closure computation, keyfinding, FD closures, minimal basis of a set)
-- relationMenu :: _ ()

-- prompt the user for a set of attributes and get its closure
-- getClosure :: _ ()

-- get the keys of the currently selected relation
-- getKeys :: _ ()

-- get the set of FD closures, printing a few at a time and waiting for user approval to print more
-- getFDClosure :: _ ()

-- display the minimal basis of the FD set of the selected relation
-- minimalBasis :: _ ()

-- add an FD to a relation
-- addFD :: _ ()

-- remove an FD from a relation
-- removeFD :: _ ()

-- display the decomposition/normalization menu
-- likely commands here: check normal forms, perform arbitrary decomposition,
-- decompose to normal form, check if selected relations are lossless of another
-- decomposeMenu :: _ ()

-- prompt the user to select new relations from the known relations
-- selectRelations :: _ [Relation]

-- prompt the user for an arbitrary schema list to split a selected relation into
-- decompose :: _ ()

-- print the list of normal forms the selected relation is in
-- normalForms :: _ ()

-- perform an arbitrary decomposition to a new normal form
-- normalize :: _ ()

-- probably others too, but this isn't really the main focus at the moment and I want to ask about how to do the
-- state of the REPL loop before committing to anything