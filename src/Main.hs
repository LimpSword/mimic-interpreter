module Main where

import Control.Monad (void)
import Text.Megaparsec
import Data.Text (Text, pack)
import Data.Void
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Maybe (maybeToList)
import Text.Megaparsec.Char
import Control.Monad.State
import System.Exit
import System.Environment

data Expr 
    = Num Double
    | Ident Text
    | Bool Bool
    | String Text

-- Just to make the output more readable (instead of showing the constructor)
instance Show Expr where
    show (Num n) = show n
    show (Ident i) = show i
    show (Bool b) = show b
    show (String s) = show s

-- Every possible statement in the language
data Stmt 
    = Skip
    | Print Expr
    | Panic Expr
    | Assign Text Expr
    | If Expr Block (Maybe Block)
    | While Expr Block
    deriving Show

data Block 
    = Void
    | Statement [Stmt]
    deriving Show

-- Store to keep track of variables while interpreting
data Store = Store [(Text, Expr)]
    deriving Show

type Parser = Parsec Void Text
type Interpreter a = StateT Store IO a

reservedWords :: [String]
reservedWords = ["if", "else", "then", "skip", "print", "true", "false", "panic", "while"]

sc :: Parser ()
sc = L.space space1 (L.skipLineComment "//") (L.skipBlockComment "/*" "*/")

-- Helper function to parse reserved words if they are not part of a larger identifier
rword :: Text -> Parser ()
rword w = string w *> notFollowedBy alphaNumChar *> sc

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

num :: Parser Expr
num = Num <$> L.signed sc (lexeme L.decimal)

-- An identifier is a letter followed by zero or more alphanumeric characters
identText :: Parser Text
identText = lexeme $ do 
    x <- letterChar
    xs <- many alphaNumChar
    return $ pack (x:xs)

ident :: Parser Expr
ident = Ident <$> identText

bool :: Parser Expr
bool = Bool <$> (True <$ rword "true" <|> False <$ rword "false")

str :: Parser Expr
str = String <$> (char '"' >> pack <$> manyTill L.charLiteral (char '"'))

-- Try to parse a number, then a boolean, then a string, then an identifier
expr :: Parser Expr
expr = try num <|> try bool <|> try str <|> ident

skip :: Parser Stmt
skip = Skip <$ rword "skip"

panic :: Parser Stmt
panic = do
    _ <- rword "panic"
    Panic <$> str

assign :: Parser Stmt
assign = do
    var <- identText
    -- Check if the variable name is a reserved word, if so, fail
    if var `elem` map pack reservedWords
        then fail $ "Keyword " ++ show var ++ " cannot be used as a variable name"
        else do
            _ <- symbol ":="
            Assign var <$> expr

printStmt :: Parser Stmt
printStmt = Print <$> (symbol "print" >> expr)

ifStmt :: Parser Stmt
ifStmt = do
    _ <- rword "if"
    cond <- expr
    _ <- rword "then"
    _ <- symbol "{"
    thenBlock <- block
    _ <- symbol "}"
    elseBlock <- optional $ do
        _ <- symbol "else"
        _ <- symbol "{"
        b <- block
        _ <- symbol "}"
        return b
    return $ If cond thenBlock elseBlock

whileStmt :: Parser Stmt
whileStmt = do
    _ <- rword "while"
    cond <- expr
    _ <- symbol "{"
    block <- block
    _ <- symbol "}"
    return $ While cond block

-- Try to parse a statement in the following order: print, panic, skip, if, while, assign
stmt :: Parser Stmt
stmt = try printStmt <|> try panic <|> try skip <|> try ifStmt <|> try whileStmt <|> assign

block :: Parser Block
block = Statement <$> sepEndBy1 stmt (symbol ";") <|> return Void


initStore :: Store
initStore = Store []

lookupVar :: Text -> Interpreter (Maybe Expr)
lookupVar var = do
    -- get the current store (StateT Store IO)
    (Store store) <- get
    return $ lookup var store

-- Update or add a variable to the store
updateStore :: Text -> Expr -> Interpreter ()
updateStore var val = do
    -- get the current store (StateT Store IO)
    (Store store) <- get
    let newStore = (var, val) : filter (\(v,_) -> v /= var) store
    -- put the new store back in the state
    put (Store newStore)

evalExpr :: Expr -> Interpreter Expr
-- Try to evaluate an identifier by looking it up in the store
evalExpr (Ident var) = do
    val <- lookupVar var
    case val of
        Just v -> return v
        Nothing -> errorWithoutStackTrace $ "Undefined variable: " ++ show var
-- Otherwise, just return the value
evalExpr e = return e

-- Evaluate a single statement
evalStmt :: Stmt -> Interpreter ()
evalStmt Skip = return ()
evalStmt (Panic expr) = do
    val <- evalExpr expr
    liftIO $ putStrLn $ "Panic: " ++ show val
    liftIO $ exitWith (ExitFailure 1) -- exit after printing panic message
evalStmt (Assign var expr) = do
    val <- evalExpr expr
    updateStore var val
evalStmt (If condExpr thenBlock elseBlock) = do
    cond <- evalExpr condExpr
    case cond of
        Bool True -> evalBlock thenBlock
        Bool False -> maybe (return ()) evalBlock elseBlock
        _ -> errorWithoutStackTrace "Condition must be a boolean"
evalStmt (While condExpr block) = do
    cond <- evalExpr condExpr
    case cond of
        Bool True -> evalBlock block >> evalStmt (While condExpr block)
        Bool False -> return ()
        _ -> errorWithoutStackTrace "Condition must be a boolean"
evalStmt (Print expr) = do
    val <- evalExpr expr
    liftIO $ print val

evalBlock :: Block -> Interpreter ()
evalBlock Void = return ()
evalBlock (Statement stmts) = mapM_ evalStmt stmts -- evaluate each statement in the block

runInterpreter :: Block -> IO Store
runInterpreter block = do
    (_, finalStore) <- runStateT (evalBlock block) initStore
    return finalStore

interpretProgram :: Block -> IO ()
interpretProgram block = do
    finalStore <- runInterpreter block
    putStrLn "Final Store:"
    print finalStore

readProg :: FilePath -> IO Text
readProg path = pack <$> readFile path

main :: IO ()
main = do
    -- Get the file name from the command line arguments
    file <- getArgs
    prog <- readProg (head file)
    let parsed = parse block "" prog
    case parsed of
        Left err -> print err
        Right block -> interpretProgram block
