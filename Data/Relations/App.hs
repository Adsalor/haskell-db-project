module Data.Relations.App where

import Data.Relations
import Data.Relations.Dependencies ( keysOf, combineBasis, toBasis, minimize, attrClosure )
import Data.Relations.Decomposition ( decompose2NF, decompose3NF, decomposeBCNF, projectDependencies, isLossless, isDependencyPreserving )
import Data.Relations.Normalization ( is1NF, is2NF, is3NF, isBCNF )


import Data.Text (strip, pack, unpack)
import Data.Char (isSpace)

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Token
import Text.Parsec.Language (emptyDef)

import Data.Set qualified as S
import Data.Map qualified as M

import Control.Monad (unless, foldM, ap, zipWithM_)
import Data.Either (partitionEithers)

-- Parsing --

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

schema :: Parser (String,Schema)
schema = do
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
    (id,sch) <- schema
    fds <- many (try fd)
    return (id,Rel sch (S.fromList fds))

fileParser :: Parser [(String,Relation)]
fileParser = do
    x <- many1 relation
    eof
    return x

-- utility for testing
extract :: Parser a -> String -> a
extract p s = either (error . show) id $ parse p "" s



------------------------- APP CODE ------------------------------

type Namespace = M.Map String Relation

-- entry point to the program
-- initializes the main loop with an empty namespace
main :: IO ()
main = mainLoop M.empty

-- list of commands for the main menu
mainCommands :: M.Map String (Namespace -> IO Namespace)
mainCommands = M.fromList [("h",printHelp),("i",importData),("l",listRelations),("v",viewRelation),("e",editRelation),("c",decompCheck)]

-- the main program menu
mainLoop :: Namespace -> IO ()
mainLoop namespace = do
    i <- getUserInput "Select an action: "
    unless (i == "q") $
        case M.lookup i mainCommands of
            (Just fn) -> mainLoop =<< fn namespace
            Nothing -> do
                putStrLn $ "Unknown action '" ++ i ++ "', use 'h' for help."
                mainLoop namespace

-- prompt the user and then clean and return their input
getUserInput :: String -> IO String
getUserInput prompt = do
    putStr prompt
    unpack . strip . pack <$> getLine -- OverloadedStrings is not working so this is the workaround

-- display a help menu
printHelp :: Namespace -> IO Namespace
printHelp namespace = do
    putStrLn "h: help menu"
    putStrLn "i: import new relations"
    putStrLn "l: list existing relations"
    putStrLn "v: view relation details"
    putStrLn "e: edit existing relation"
    putStrLn "c: check decomposition properties"
    putStrLn "q: quit this app"
    return namespace

-- display a named relation
showRel :: String -> Relation -> IO ()
showRel name relation = putStrLn $ name ++ show relation

-- list all relations
listRelations :: Namespace -> IO Namespace
listRelations namespace = do
    if null namespace then
        putStrLn "No current relations!"
    else
        sequence_ $ M.mapWithKey showRel namespace
    return namespace

-- list of commands for the import submenu
importCommands :: M.Map String (Namespace -> IO Namespace)
importCommands = M.fromList [("f",loadFromFile),("i",readManualRelation),("c",return)]

-- importing data can happen by file or by manual entry
importData :: Namespace -> IO Namespace
importData namespace = do
    i <- getUserInput "Select a loading option [f for file, i for input, c to cancel]: "
    case M.lookup i importCommands of
        (Just fn) -> fn namespace
        Nothing -> do
            putStrLn $ "Unknown action '" ++ i ++ "', please use one of the listed options!"
            importData namespace

-- load in data from a file
-- if necessary, prompt the user to resolve namespace conflicts
loadFromFile :: Namespace -> IO Namespace
loadFromFile namespace = do
    i2 <- getUserInput "Input the filename: "
    contents <- parseFromFile fileParser i2
    case contents of
        (Left err) -> do
            putStrLn $ "Error parsing your input: " ++ show err
            return namespace
        (Right list) ->
            mergeInto list namespace

-- parse user input, and prompt them to resolve namespace conflicts if needed
readManualRelation :: Namespace -> IO Namespace
readManualRelation namespace = do
    i2 <- getUserInput "Input the new relation: "
    case parse relation "" i2 of
        (Left err) -> do
            putStrLn $ "Error parsing your input: " ++ show err
            return namespace
        (Right (name,rel)) -> do
            unless (M.member name namespace) $ putStrLn $ "Successfully imported relation '" ++ name ++ "'!"
            mergeRel namespace (name,rel)

-- merge a list of imported relations into the namespace
-- prompt the user to resolve conflicts as needed
mergeInto :: [(String, Relation)] -> Namespace -> IO Namespace
mergeInto list namespace = do
    ls <- foldM mergeRel namespace list
    putStrLn "Successfully loaded in all relations from file!"
    return ls

-- merge a single relation into the namespace by getting new names if needed
mergeRel :: Namespace -> (String, Relation) -> IO Namespace
mergeRel namespace (name,rel) = if M.member name namespace then do
        putStrLn $ "'" ++ name ++ "' is already in use!"
        newName rel namespace
    else return $ M.insert name rel namespace

-- get a new name from the user for a relation and place it into the namespace with the new name
newName :: Relation -> Namespace -> IO Namespace
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

-- view details of a specific relation
-- this function serves as a way to ensure the user does not get softlocked
-- by guaranteeing they have relations before entering the actual details menu
viewRelation :: Namespace -> IO Namespace
viewRelation namespace = do
    if null namespace then putStrLn "No relations known! Load some first!"
    else do
        (name,rel) <- getUserRelation namespace
        showDetails name rel
    return namespace

-- prompt the user to select a relation and go until they specify a valid name
-- if this function is called with an empty namespace it will softlock the user
-- so check that the namespace is nonempty first
getUserRelation :: Namespace -> IO (String,Relation)
getUserRelation namespace = do
    i <- getUserInput "Please input the name of the relation: "
    case M.lookup i namespace of
        (Just rel) -> return (i,rel)
        Nothing -> do
            putStrLn $ "Unknown name '" ++ i ++ "', please specify an existing name! Known relations are"
            listRelations namespace
            getUserRelation namespace

-- list of commands for the detailed view submenu
viewCommands :: M.Map String (String -> Relation -> IO ())
viewCommands = M.fromList [("a",showAll),("n",showNormalForm),("k",showKeys),("c",showClosure)]

-- prompt the user for a view subcommand and respond to it
showDetails :: String -> Relation -> IO ()
showDetails name rel = do
    i <- getUserInput "Select your desired details [a for all, n for normal forms, k for keys, c for attribute closure]: "
    case M.lookup i viewCommands of
        (Just fn) -> fn name rel
        Nothing -> do
            putStrLn $ "Unknown action '" ++ i ++ "', please use one of the listed options!"
            showDetails name rel

-- show all details of a relation: name/schema, keys, normal forms
showAll :: String -> Relation -> IO ()
showAll name rel = do
    showRel name rel
    showKeys name rel
    showNormalForm name rel

-- show normal forms of a relation
showNormalForm :: String -> Relation -> IO ()
showNormalForm name rel = do
    putStrLn $ "Normal Forms of " ++ name ++ ":"
    putStrLn $ "1NF: " ++ show (is1NF rel) ++ "   2NF: " ++ show (is2NF rel)
    putStrLn $ "3NF: " ++ show (is3NF rel) ++ "   BCNF: " ++ show (isBCNF rel)

-- show keys of a relation
-- this is a bit lazy in that it uses the List show instance
-- instead of anything prettier
showKeys :: String -> Relation -> IO ()
showKeys name rel = do
    putStr $ "Keys of " ++ name ++ ": "
    print $ map S.toAscList $ S.toAscList $ keysOf rel

-- prompt the user for multiple sets of attributes they want the closure of
-- then display the closures
-- if there are unknown attributes in their input respond to that as well
showClosure :: String -> Relation -> IO ()
showClosure name rel@(Rel sch _) = do
    showRel name rel
    i <- getUserInput "Please list all attribute sets you want the closures of: "
    case parse (many1 (S.fromList . map Attr <$> (ident `sepBy1` sym ","))) "" i of
        (Left err) -> putStrLn $ "Error parsing your input: " ++ show err
        (Right attrs) -> do
            let unknowns = S.unions $ map (S.filter (`notElem` sch)) attrs
            if null unknowns then
                mapM_ (print . ap To (attrClosure rel)) attrs
            else do
                putStrLn $ "Unknown attributes " ++ show (S.toList unknowns) ++ " in input!"

-- list of commands for the edit submenu
editCommands :: M.Map String (String -> Relation -> Namespace -> IO Namespace)
editCommands = M.fromList [("a",addFDs),("r",removeFDs),("m",minimizeRel),("d",decompose),("e",eraseRel),("rn",renameRel),("c",const $ const return)]

-- entry point for the edit submenu
-- similarly to viewRelation, this serves as a guard against softlocks
editRelation :: Namespace -> IO Namespace
editRelation namespace = if null namespace then do
        putStrLn "No relations to edit! Load some first!"
        return namespace
    else do
        editSelectedRelation namespace =<< getUserRelation namespace

-- once the user has selected a relation, prompt them for the editing command
-- and respond appropriately
editSelectedRelation :: Namespace -> (String,Relation) -> IO Namespace
editSelectedRelation namespace (name,rel) = do
    putStrLn "[a to add FDs, r to remove FDs, m to minimize FDs, d to decompose, e to erase relation, rn to rename relation, c to cancel]"
    i <- getUserInput "Please choose an action: "
    case M.lookup i editCommands of
        (Just fn) -> fn name rel namespace
        Nothing -> do
            putStrLn $ "Unknown command '" ++ i ++ "', please use a listed option!"
            editSelectedRelation namespace (name,rel)

-- prompt the user for FDs to add to an existing relation
-- uses set union semantics, but combines as neatly as possible
-- so impossible to have both A->B and A->BC after adding
addFDs :: String -> Relation -> Namespace -> IO Namespace
addFDs name relation namespace = do
    i <- getUserInput "Please list all new FDs to add to the relation: "
    case parse (many1 (try fd)) "" i of
        (Left err) -> do
            putStrLn $ "Error parsing your input: " ++ show err
            return namespace
        (Right fds) -> do
            putStrLn $ "Added your FDs to relation '" ++ name ++ "'!"
            return $ M.adjust (\(Rel sch oldFDs) -> Rel sch (combineBasis $ S.union oldFDs $ S.fromList fds)) name namespace

-- prompt the user to remove FDs from a relation
-- similarly to addFDs, this uses set semantics
-- if a relation contains A->BC and the user removes A->B,
-- the relation will be left with A->C
removeFDs :: String -> Relation -> Namespace -> IO Namespace
removeFDs name relation namespace = do
    i <- getUserInput "Please list all FDs you want to remove: "
    case parse (many1 (try fd)) "" i of
        (Left err) -> do
            putStrLn $ "Error parsing your input: " ++ show err
            return namespace
        (Right fds) -> do
            putStrLn $ "Added your FDs to relation '" ++ name ++ "'!"
            return $ M.adjust (\(Rel sch oldFDs) -> Rel sch (combineBasis $ S.difference (toBasis oldFDs) $ toBasis $ S.fromList fds)) name namespace

-- minimize the relation the user input
-- note that this is purely for convenience as in all cases the non-minimized basis
-- carries the same meaning
minimizeRel :: String -> Relation -> Namespace -> IO Namespace
minimizeRel name _ namespace = do
    putStrLn $ "Minimized FDs of relation '" ++ name ++ "'!"
    return $ M.adjust (\(Rel sch fds) -> Rel sch $ combineBasis $ minimize fds) name namespace

-- rename a relation and insert the new name into the namespace
renameRel :: String -> Relation -> Namespace -> IO Namespace
renameRel oldName relation namespace = newName relation (M.delete oldName namespace)

-- list of decomposition modes
decomposeCommands :: M.Map String (String -> Relation -> Namespace -> IO Namespace)
decomposeCommands = M.fromList [
    ("input",decomposeArbitrary),
    ("2NF",liftDecomposition decompose2NF),
    ("3NF",liftDecomposition decompose3NF),
    ("BCNF",liftDecomposition decomposeBCNF),
    ("c",const $ const return)]

-- allow the user to list an arbitrary decomposition to decompose to
-- they can input any names, but namespace conflicts will be resolved late in insertion
decomposeArbitrary :: String -> Relation -> Namespace -> IO Namespace
decomposeArbitrary name rel@(Rel sch fds) namespace = do
    i <- getUserInput "Please list the relations to decompose into: "
    case parse (many1 schema) "" i of
        (Left err) -> do
            putStrLn $ "Error parsing your input: " ++ show err
            return namespace
        (Right schs) -> do
            let unknowns = S.unions $ map (S.filter (`notElem` sch) . snd) schs
            if null unknowns then do
                let projected = map (fmap $ projectDependencies fds) schs
                mergeInto projected namespace
            else do
                putStrLn $ "Unknown attributes " ++ show (S.toList unknowns) ++ " in input!"
                return namespace

-- add a decomposed relation to the namespace by prompting the user to replace the old name
-- and reacting accordingly
-- the initially generated name is guaranteed to be unique, but not necessarily good
addNewName :: String -> Namespace -> (Relation, Int) -> IO Namespace
addNewName origName namespace (rel,num) = do
    let relName = head $ dropWhile (`M.member` namespace) $ iterate (++ "'") $ origName ++ show num
    showRel relName rel
    i <- getUserInput $ "Rename " ++ relName ++ "? [y/n]: "
    if i == "y" then
        newName rel namespace
    else
        return $ M.insert relName rel namespace

-- lift a pure decomposition function into an IO action
-- generates initial names for the decomposed relations 
-- and prompts the user to rename them one-by-one
-- renamed decomposed relations are added to the namespace
liftDecomposition :: (Relation -> [Relation]) -> String -> Relation -> Namespace -> IO Namespace
liftDecomposition fn name rel namespace = do
    let decomp = fn rel
    if 1 == length decomp then do
        putStrLn $ name ++ " was already in that normal form!"
        return namespace
    else
        foldM (addNewName name) namespace (zip decomp [1..])

-- look up the user's decomposition mode and respond accordingly
decompose :: String -> Relation -> Namespace -> IO Namespace
decompose name relation namespace = do
    i <- getUserInput "Select decomposition mode, or c to cancel: "
    case M.lookup i decomposeCommands of
        (Just fn) -> fn name relation namespace
        Nothing -> do
            putStrLn "Unknown decomposition mode! Use input, 2NF, 3NF, or BCNF!"
            decompose name relation namespace

-- erase a relation from the namespace
eraseRel :: String -> Relation -> Namespace -> IO Namespace
eraseRel name _ namespace = do
    let newNamespace = M.delete name namespace
    putStrLn $ "Erased relation '" ++ name ++ "'."
    return newNamespace

-- Decomposition check 

-- lifts a Maybe into an Either where the Left is function input
-- frankly I'm surprised some function-agnostic version of this isn't
-- already an Either combinator
retrieveFrom :: Namespace -> String -> Either String Relation
retrieveFrom namespace name =
    case M.lookup name namespace of
        (Just x) -> Right x
        Nothing -> Left name

-- allow the user to list names in a decomposition and then check properties of the decomposition
decompCheck :: Namespace -> IO Namespace
decompCheck namespace = do
    (name,rel) <- getUserRelation namespace
    rs <- getUserInput "Please list the names of relations in the decomposition: "
    case parse (many1 ident) "" rs of
        (Left err) -> do
            putStrLn $ "Error parsing your input: " ++ show err
        (Right rs) -> do
            let (unknowns,knowns) = partitionEithers $ map (retrieveFrom namespace) rs
            if null unknowns then do
                putStr "Original: "
                showRel name rel
                putStrLn "Decomposited: "
                zipWithM_ showRel rs knowns
                putStrLn $ "Lossless? " ++ show (isLossless rel knowns)
                putStrLn $ "Dependency preserving? " ++ show (isDependencyPreserving rel knowns)
            else do
                putStrLn $ "Unknown relations '" ++ unwords unknowns ++ "' in your input"
    return namespace