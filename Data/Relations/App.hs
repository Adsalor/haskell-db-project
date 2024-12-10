module Data.Relations.App where

import Data.Relations
import Data.Relations.Dependencies
import Data.Relations.Decomposition
import Data.Relations.Normalization


import Data.Text (strip, pack, unpack)
import Data.Char (isSpace)

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Token
import Text.Parsec.Language (emptyDef)

import Data.Set qualified as S
import Data.Map qualified as M

import Control.Monad (unless, foldM, join)

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

-- not doing selection because it's a pain in the ass
-- -- -- -- s <relationName> - select relation
-- -- -- -- add <L>-><R> - add a FD to the relation (L and R are in the format of the inputFile)
-- -- -- -- remove <L>-><R> - remove a FD to the relation if it exists
-- -- -- -- check <decompType> - check if currently selected relation follows a normal form
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

main :: IO ()
main = mainLoop M.empty

mainCommands :: M.Map String (M.Map String Relation -> IO (M.Map String Relation))
mainCommands = M.fromList [("h",printHelp),("i",importData),("l",listRelations),("v",viewRelation),("e",editRelation)]

mainLoop :: M.Map String Relation -> IO ()
mainLoop namespace = do
    i <- getUserInput "Select an action: "
    unless (i == "q") $
        case M.lookup i mainCommands of
            (Just fn) -> do
                mainLoop =<< fn namespace
            Nothing -> do
                putStrLn ("Unknown action '" ++ i ++ "', use 'h' for help")
                mainLoop namespace

getUserInput :: String -> IO String
getUserInput prompt = do
    putStr prompt
    unpack . strip . pack <$> getLine -- OverloadedStrings is not working so this is the workaround

printHelp :: M.Map String Relation -> IO (M.Map String Relation)
printHelp namespace = do
    putStrLn "h: help menu"
    putStrLn "i: import new relations"
    putStrLn "l: list existing relations"
    putStrLn "v: view relation details"
    putStrLn "e: edit existing relation"
    putStrLn "q: quit this app"
    return namespace

showRel :: String -> Relation -> IO ()
showRel name relation = putStrLn $ name ++ show relation

listRelations :: M.Map String Relation -> IO (M.Map String Relation)
listRelations namespace = do
    if null namespace then
        putStrLn "No current relations!"
    else
        sequence_ $ M.mapWithKey showRel namespace
    return namespace

importCommands :: M.Map String (M.Map String Relation -> IO (M.Map String Relation))
importCommands = M.fromList [("f",loadFromFile),("i",readManualRelation),("c",return)]

importData :: M.Map String Relation -> IO (M.Map String Relation)
importData namespace = do
    i <- getUserInput "Select a loading option [f for file, i for input, c to cancel]: "
    case M.lookup i importCommands of
        (Just fn) -> fn namespace
        Nothing -> do
            putStrLn ("Unknown action '" ++ i ++ "', please use one of the listed options!")
            importData namespace

loadFromFile :: M.Map String Relation -> IO (M.Map String Relation)
loadFromFile namespace = do
    i2 <- getUserInput "Input the file: "
    contents <- parseFromFile fileParser i2
    case contents of
        (Left err) -> do
            putStrLn $ "Error parsing your input: " ++ show err
            return namespace
        (Right list) ->
            mergeInto list namespace

readManualRelation :: M.Map String Relation -> IO (M.Map String Relation)
readManualRelation namespace = do
    i2 <- getUserInput "Input the new relation: "
    case parse relation "" i2 of
        (Left err) -> do
            putStrLn $ "Error parsing your input: " ++ show err
            return namespace
        (Right (name,rel)) -> do
            unless (M.member name namespace) $ putStrLn $ "Successfully imported relation '" ++ name ++ "'!"
            mergeRel namespace (name,rel)

mergeInto :: [(String, Relation)] -> M.Map String Relation -> IO (M.Map String Relation)
mergeInto list namespace = do
    ls <- foldM mergeRel namespace list
    putStrLn "Successfully loaded in all relations from file!"
    return ls

mergeRel :: M.Map String Relation -> (String, Relation) -> IO (M.Map String Relation)
mergeRel namespace (name,rel) = do
    if M.member name namespace then do
        putStrLn $ "'" ++ name ++ "' is already in use!"
        newName rel namespace
    else
        return $ M.insert name rel namespace

newName :: Relation -> M.Map String Relation -> IO (M.Map String Relation)
newName rel namespace = do
    i <- getUserInput "Please input a new name: "
    let parsedName = parse ident "" i
    case parse ident "" i of
        (Right name) ->
            if M.member name namespace then do
                putStrLn $ "'" ++ name ++ "' is already in use!"
                newName rel namespace
            else do
                putStrLn $ "Successfully inserted relation " ++ name ++ "!"
                return $ M.insert name rel namespace
        (Left err) -> do
            putStrLn "Invalid formatting! Names should be one word."
            newName rel namespace

viewRelation :: M.Map String Relation -> IO (M.Map String Relation)
viewRelation namespace = do
    if null namespace then do
        putStrLn "No relations known! Load some first!"
        return namespace
    else do
        (name,rel) <- getUserRelation namespace
        showDetails name rel
        return namespace

getUserRelation :: M.Map String Relation -> IO (String,Relation)
getUserRelation namespace = do
    i <- getUserInput "Please input the name of the relation: "
    case M.lookup i namespace of
        (Just rel) -> do
            return (i,rel)
        Nothing -> do
            putStrLn $ "Unknown name '" ++ i ++ "', please specify an existing name! Known relations are"
            listRelations namespace
            getUserRelation namespace

showDetails :: String -> Relation -> IO ()
showDetails name rel = do
    print rel
    putStr $ "Keys of " ++ name ++ ": "
    print $ map S.toAscList $ S.toAscList $ keysOf rel
    putStrLn "Normal Forms:"
    putStrLn $ "1NF: " ++ show (is1NF rel) ++ "   2NF: " ++ show (is2NF rel)
    putStrLn $ "3NF: " ++ show (is3NF rel) ++ "   BCNF: " ++ show (isBCNF rel)

editCommands :: M.Map String (String -> Relation -> M.Map String Relation -> IO (M.Map String Relation))
editCommands = M.fromList [("a",addFDs),("r",removeFDs),("d",decompose),("c",const $ const return)]

editRelation :: M.Map String Relation -> IO (M.Map String Relation)
editRelation namespace = do
    if null namespace then do
        putStrLn "No relations to edit! Load some first!"
        return namespace
    else do
        selected <- getUserRelation namespace
        editSelectedRelation namespace selected

editSelectedRelation :: M.Map String Relation -> (String,Relation) -> IO (M.Map String Relation)
editSelectedRelation namespace (name,rel) = do
    i <- getUserInput "Please choose an action [a to add FDs, r to remove FDs, d to decompose, c to cancel]: "
    case M.lookup i editCommands of
        (Just fn) -> fn name rel namespace
        Nothing -> do
            putStrLn $ "Unknown command '" ++ i ++ "', please use a listed option!"
            editSelectedRelation namespace (name,rel)

addFDs :: String -> Relation -> M.Map String Relation -> IO (M.Map String Relation)
addFDs name relation namespace = do
    i <- getUserInput "Please list all new FDs to add to the relation!"
    case parse (many1 (try fd)) "" i of
        (Left err) -> do
            putStrLn $ "Error parsing your input: " ++ show err
            return namespace
        (Right fds) -> do
            putStrLn $ "Added your FDs to relation '" ++ name ++ "'!"
            return $ M.adjust (\(Rel sch oldFDs) -> Rel sch (S.union oldFDs $ S.fromList fds)) name namespace

removeFDs :: String -> Relation -> M.Map String Relation -> IO (M.Map String Relation)
removeFDs = undefined -- todo later :(

renameRel :: String -> Relation -> M.Map String Relation -> IO (M.Map String Relation)
renameRel oldName relation namespace = newName relation (M.delete oldName namespace)

decomposeCommands :: M.Map String (Relation -> [Relation])
decomposeCommands = M.fromList [("2NF",decompose2NF),("3NF",decompose3NF),("BCNF",decomposeBCNF),("c",return)]

decompose :: String -> Relation -> M.Map String Relation -> IO (M.Map String Relation)
decompose name relation namespace = do
    i <- getUserInput "Select decomposition mode, or c to cancel: "
    case M.lookup i decomposeCommands of
        (Just fn) -> do
            let decomp = fn relation
            if 1 == length decomp then do
                putStrLn $ "Relation was already in " ++ i ++ "!"
                return namespace
            else
                return $ foldr (addNewName name) namespace (zip decomp [1..])
        Nothing -> do
            putStrLn "Unknown decomposition mode! Use 2NF, 3NF, or BCNF!"
            decompose name relation namespace

addNewName :: String -> (Relation, Int) -> M.Map String Relation -> M.Map String Relation
addNewName origName (rel,num) namespace =
    let relName = head $ dropWhile (`M.member` namespace) $ iterate (++ "'") $ origName ++ show num
    in M.insert relName rel namespace

