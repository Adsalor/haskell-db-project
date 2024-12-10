module Data.Relations.App where

import Data.Relations
import Data.Relations.Dependencies
import Data.Relations.Decomposition
import Data.Relations.Normalization

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Token
import Text.Parsec.Language (emptyDef)
import Data.Set qualified as S

-- Parsing

lexer = makeTokenParser $ emptyDef {
    commentLine = "--",
    identStart = alphaNum,
    identLetter = alphaNum
}
ident :: Parser String
ident = identifier lexer
sym :: String -> Parser String
sym = symbol lexer
wsp :: Parser ()
wsp = whiteSpace lexer

name :: Parser (String,Schema)
name = do
    rel <- ident
    sym "("
    attrs <- ident `sepBy1` sym ","
    sym ")"
    wsp
    return (rel,S.fromList (map Attr attrs))

fd :: Parser FunctionalDependency
fd = do
    lhs <- ident `sepBy1` sym ","
    sym "->"
    rhs <- ident `sepBy1` sym ","
    wsp
    return $ S.fromList (map Attr lhs) `To` S.fromList (map Attr rhs)

relation :: Parser (String,Relation)
relation = do
    (id,sch) <- name
    fds <- many (try fd)
    return (id,Rel sch (S.fromList fds))

fileParser :: Parser [(String,Relation)]
fileParser = do
    x <- many1 relation
    eof
    return x

-- temporary parsing utility
extract :: Parser a -> String -> a
extract p s = either (error . show) id $ parse p "" s

-- h - List of commands
-- l <filename> - Load File
-- db - List all relation names
-- r <relationName> - view relation
-- k <relationName> - get keys of a relation
-- n <RelationName> - add a new relation
-- s <relationName> - select relation
-- add <L>-><R> - add a FD to the relation (L and R are in the format of the inputFile)
-- remove <L>-><R> - remove a FD to the relation if it exists
-- check <decompType> - check if currently selected relation follows a normal form
    -- decompType is '1NF' '2NF' '3NF' 'BCNF'
    -- if no normal form is specified, list all normal forms the selected relation follows
-- decomp <decompType> - decompose selected relation into a normal form
    -- should add the decomposed relations to the environment i imagine
-- min - display the minimal basis of the selected relation
-- closure <list of attributes> - give the closure of the list of attributes in the selected relation

-- is decomposition <Decomposition> - Given a list of relations in the form of <Name>(<Attributes>) separated by commas, create the relations in the decomposition (including projected relations)
    -- Ex - decomposition R1(A,B),R2(C,E),R3(A,D)
-- lossless - check if a set of relations is a lossless decomposition of the selected relation
-- preserving - check if a set of relations is a dependency preserving decompositon of the selected relations


-- I think the way things like checking if a set of relations is a lossless decomposition of another could work like this
    -- I think theres probably a better way to go about this part so if you have suggestions lmk
    -- You call the command with the non-decomposed relation selected, then it prompts you to a second menu
    -- This menu only has the ability to list all relations, select relations to add to the decomposition set, or cancel
    -- so for example something like

    -- Current Relation: R
    --  -> lossless (the arrow is just what im imagining to demonstrate that its taking user input)
    -- Choose what relations make up the decomposition of R: select h for help
    --  -> h
    --  h - List of commands
    --  db - List all relation names and schemas (maybe limit it to ones that have a subset of the attributes so that its not cluttering with relations that obviously cant be part of the decomp?)
    --  r <Relations> - Select relations to make up the decomposition of R, these should be the names of the relations separated with commas and no spaces
    --  c - Cancel operation
    --  -> r R1,R2,R3
    --  True
    -- Current Relation: R (This is the outer menu again, theyd have to select lossless again to try a new set)







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