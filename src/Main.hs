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

data Expr 
    = Num Double
    | Ident Text
    | Bool Bool
    | String Text

instance Show Expr where
    show (Num n) = show n
    show (Ident i) = show i
    show (Bool b) = show b
    show (String s) = show s

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

data Store = Store [(Text, Expr)]
    deriving Show

type Parser = Parsec Void Text
-- Interpreter State Monad
type Interpreter a = StateT Store IO a

reservedWords :: [String]
reservedWords = ["if", "else", "then", "skip", "print", "true", "false", "panic", "while"]

sc :: Parser ()
sc = L.space space1 (L.skipLineComment "//") (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

num :: Parser Expr
num = Num <$> lexeme L.decimal

identText :: Parser Text
identText = (lexeme . try) $ do
    x <- letterChar
    xs <- many alphaNumChar
    let ident = x:xs
    if ident `elem` reservedWords
        then fail $ "keyword " ++ ident ++ " cannot be an identifier"
        else return $ Data.Text.pack ident

ident :: Parser Expr
ident = Ident <$> identText

bool :: Parser Expr
bool = Bool <$> (True <$ symbol "true" <|> False <$ symbol "false")

str :: Parser Expr
str = String <$> (char '"' >> pack <$> manyTill L.charLiteral (char '"'))

expr :: Parser Expr
expr = num <|> ident <|> bool <|> str

skip :: Parser Stmt
skip = Skip <$ symbol "skip"

panic :: Parser Stmt
panic = do
    _ <- symbol "panic"
    Panic <$> str

assign :: Parser Stmt
assign = do
    var <- identText
    _ <- symbol ":="
    Assign var <$> expr

printStmt :: Parser Stmt
printStmt = Print <$> (symbol "print" >> expr)

ifStmt :: Parser Stmt
ifStmt = do
    _ <- symbol "if"
    cond <- expr
    _ <- symbol "then"
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
    _ <- symbol "while"
    cond <- expr
    _ <- symbol "{"
    block <- block
    _ <- symbol "}"
    return $ While cond block

stmt :: Parser Stmt
stmt = panic <|> printStmt <|> skip <|> ifStmt <|> whileStmt <|> assign 

block :: Parser Block
block = Statement <$> sepEndBy1 stmt (symbol ";") <|> return Void


-- Initialize an empty store
initStore :: Store
initStore = Store []

-- Lookup a variable in the store
lookupVar :: Text -> Interpreter (Maybe Expr)
lookupVar var = do
    (Store store) <- get
    return $ lookup var store

-- Update or add a variable to the store
updateStore :: Text -> Expr -> Interpreter ()
updateStore var val = do
    (Store store) <- get
    let newStore = (var, val) : filter (\(v,_) -> v /= var) store
    put (Store newStore)

-- Evaluate an expression
evalExpr :: Expr -> Interpreter Expr
evalExpr e@(Num _) = return e
evalExpr e@(Bool _) = return e
evalExpr e@(String _) = return e
evalExpr (Ident var) = do
    val <- lookupVar var
    case val of
        Just v -> return v
        Nothing -> error $ "Undefined variable: "

-- Evaluate a single statement
evalStmt :: Stmt -> Interpreter ()
evalStmt Skip = return ()
evalStmt (Panic expr) = do
    val <- evalExpr expr
    liftIO $ putStrLn $ "Panic: " ++ show val
    liftIO $ exitWith (ExitFailure 1)
evalStmt (Assign var expr) = do
    val <- evalExpr expr
    updateStore var val
evalStmt (If condExpr thenBlock elseBlock) = do
    cond <- evalExpr condExpr
    case cond of
        Bool True -> evalBlock thenBlock
        Bool False -> maybe (return ()) evalBlock elseBlock
        _ -> error "Condition must be a boolean"
evalStmt (While condExpr block) = do
    cond <- evalExpr condExpr
    case cond of
        Bool True -> evalBlock block >> evalStmt (While condExpr block)
        Bool False -> return ()
        _ -> error "Condition must be a boolean"
evalStmt (Print expr) = do
    val <- evalExpr expr
    liftIO $ print val

-- Evaluate a block of statements
evalBlock :: Block -> Interpreter ()
evalBlock Void = return ()
evalBlock (Statement stmts) = mapM_ evalStmt stmts

-- Run the interpreter with initial store
runInterpreter :: Block -> IO Store
runInterpreter block = do
    (_, finalStore) <- runStateT (evalBlock block) initStore
    return finalStore

-- Helper function to run and print store state
interpretProgram :: Block -> IO ()
interpretProgram block = do
    finalStore <- runInterpreter block
    putStrLn "Final Store:"
    print finalStore

readProg :: FilePath -> IO Text
readProg path = pack <$> readFile path

main :: IO ()
main = do
    prog <- readProg "examples/example.c"
    let parsed = parse block "" prog
    case parsed of
        Left err -> print err
        Right block -> interpretProgram block
