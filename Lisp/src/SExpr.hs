module SExpr (
    SExpr(..),
    getSymbol,
    getInteger,
    getList,
    printTree,
    describeList,
    describeListRest
) where

-- SExpr represents S-expressions for a Lisp interpreter
data SExpr = Integer Int
    | Symbol String
    | List [SExpr]
    deriving (Show, Eq)

getSymbol :: SExpr -> Maybe String
getSymbol (Symbol s) = Just s
getSymbol _          = Nothing

getInteger :: SExpr -> Maybe Int
getInteger (Integer n) = Just n
getInteger _           = Nothing

getList :: SExpr -> Maybe [SExpr]
getList (List xs) = Just xs
getList _         = Nothing

printTree :: SExpr -> Maybe String
printTree (Symbol s) = Just ("a Symbol " ++ s)
printTree (Integer n) = Just ("a Number " ++ show n)
printTree (List xs) = do
    xs' <- describeList xs
    return $ "a List with " ++ xs'

describeList :: [SExpr] -> Maybe String
describeList [] = Just "nothing in it"
describeList (x:xs) = do
    x' <- printTree x
    xs' <- describeListRest xs
    return $ x' ++ " followed by " ++ xs'

describeListRest :: [SExpr] -> Maybe String
describeListRest [] = Just ""
describeListRest [x] = printTree x
describeListRest (x:xs) = do
    x' <- printTree x
    xs' <- describeListRest xs
    return $ x' ++ ", " ++ xs'
