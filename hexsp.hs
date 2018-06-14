import Data.ByteString as BS
import Data.Char as Char
import Data.Function
import Data.List as List
import Data.Map as Map
import Data.Maybe as Maybe
import Data.Text as Text
import Control.Monad
import Control.Monad.State
import Numeric (readHex)
import System.Environment (getArgs)
import System.IO as IO
import Text.Parsec
import Text.Parsec.String
import Text.Printf

data Expr = Pair (Expr, Expr) | Nil | Symbol Integer | Number Integer deriving (Eq)

--instance Show Expr where
--  show (Nil) = "Nil"
--  show (Symbol x) = "Symbol " ++ showHex x
--  show (Number x)
--    | x < 0 = "Number -" ++ showHex (negate x)
--    | otherwise = "Number " ++ showHex x
--  show p@(Pair (x, y))
--    | isList p = "(" ++ ((List.concat).(List.intersperse ", ").(List.map show).pairsToList $ p) ++ ")"
--    | otherwise = "(" ++ show x ++ ", " ++ show y ++ ")"

instance Show Expr where
  show (Nil) = "EE 33 "
  show (Symbol x) = showHex x ++ " "
  show (Number x)
    | x < 0 = "22 " ++ showHex (negate x) ++ " "
    | otherwise = "55 " ++ showHex x ++ " "
  show p@(Pair (x, y))
    | isList p = "EE " ++ ((List.concat).(List.map show).pairsToList $ p) ++ "33 "
    | otherwise = "EE " ++ show x ++ "11 " ++ show y ++ "33 "

type Env = Map Integer Object
data Object = Expr Expr | Closure Expr Env deriving (Show)

lookupEnv _ [] = Nothing
lookupEnv k env = listToMaybe $ Maybe.mapMaybe (Map.lookup k) [List.head env, List.last env]

keywords = ["00", "11", "22", "EE", "33", "55", "99", "FF"]
hexs = let h = ['0'..'9'] ++ ['A'..'F'] in [[x, y] | x <- h, y <- h]

showHex :: Integer -> String
showHex i = do
  let s = printf "%X" i
  let a = if even $ List.length s then s else '0':s
  Text.unpack $ Text.concat $ List.intersperse (Text.pack " ") $ chunksOf 2 (Text.pack a)

isPair :: Expr -> Bool
isPair (Pair _) = True
isPair _ = False

isNil :: Expr -> Bool
isNil e = e == Nil

isList :: Expr -> Bool
isList (Pair (_, Nil)) = True
isList (Pair (_, Pair (x, y))) = isList $ Pair (x, y)
isList (Pair (_, _)) = False

listToPairs :: [Expr] -> Expr
listToPairs [] = Nil
listToPairs (x:xs) = Pair (x, listToPairs xs)

pairsToList :: Expr -> [Expr]
pairsToList (Pair (x, Nil)) = [x]
pairsToList (Pair (x, y)) = x:(pairsToList y)
pairsToList _ = error "invalid pairs"

expr :: Parser Expr
expr = do
  zeroSpaces
  a <- try list <|> try pair <|> try nil <|> try quote <|> try symbol <|> try number
  zeroSpaces
  return a

zeroSpaces :: Parser ()
zeroSpaces = do
  many.try.string $ "00 "
  return ()

list :: Parser Expr
list = do
  string "EE "
  x <- many1 expr
  string "33 "
  return $ listToPairs x

pair :: Parser Expr
pair = do
  string "EE "
  x <- expr
  string "11 "
  y <- expr
  string "33 "
  return $ Pair (x, y)

nil :: Parser Expr
nil = do
  string "EE "
  string "33 "
  return $ Nil

quote :: Parser Expr
quote = do
  string "99 "
  a <- expr
  return $ listToPairs [Symbol 0x9207E0, a]

number :: Parser Expr
number = do
  sign <- (try $ string "55 " >> return id) <|> (try $ string "22 " >> return negate)
  a <- hexList
  return $ Number $ sign a

symbol :: Parser Expr
symbol = do
  a <- hexList
  return $ Symbol a

hexList :: Parser Integer
hexList = do
  xs <- many1 (try escape <|> try normal)
  return $ fst.(List.head).readHex.(List.concat) $ xs
  where
  escape = do
    string "FF "
    let l = List.map (try.string) keywords
    x <- List.foldl (<|>) (List.head l) (List.tail l)
    space
    return x
  normal = do
    let l = List.map (try.string) $ hexs List.\\ keywords
    x <- List.foldl (<|>) (List.head l) (List.tail l)
    space
    return x


eval :: Expr -> StateT [Env] IO Expr
--nil
eval Nil = do
  return Nil
--number
eval (Number x) = do
  return $ Number x
--if
eval (Pair (Symbol 0x1F, Pair (c, Pair (x, Pair (y, Nil))))) = do
  c <- eval c
  if c == Nil then eval y else eval x
--eq
eval (Pair (Symbol 0xE9, Pair (x, Pair (y, Nil)))) = do
  x <- eval x
  y <- eval y
  return $ if x == y then Symbol 0x77 else Nil
--quote
eval (Pair (Symbol 0x9207E0, Pair (x, Nil))) = do
  return x
--atom
eval (Pair (Symbol 0xA703, Pair (x, Nil))) = do
  x <- eval x
  return $ case x of
    Pair (_, _) -> Nil
    _ -> Symbol 0x77
--car
eval (Pair (Symbol 0xCA70, Pair (x, Nil))) = do
  Pair (a, b) <- eval x
  return a
--cdr
eval (Pair (Symbol 0xCD70, Pair (x, Nil))) = do
  Pair (a, b) <- eval x
  return b
--cons
eval (Pair (Symbol 0xC025, Pair (x, Pair (y , Nil)))) = do
  x <- eval x
  y <- eval y
  return $ Pair (x, y)
--add
eval (Pair (Symbol 0xADD0, Pair (x, Pair (y , Nil)))) = do
  (Number x) <- eval x
  (Number y) <- eval y
  return $ Number $ x + y
--sub
eval (Pair (Symbol 0x52B0, Pair (x, Pair (y , Nil)))) = do
  (Number x) <- eval x
  (Number y) <- eval y
  return $ Number $ x - y
--begin
eval (Pair (Symbol 0xBE9120, Pair (x, Nil))) = do
  let prog = pairsToList x
  v <- mapM eval prog
  return $ List.last v
--write
eval (Pair (Symbol 0x3717E0, Pair (x, Nil))) = do
  v <- eval x
  lift $ print v
  return v
--setq
eval (Pair (Symbol 0x5E79, Pair (Symbol x, Pair (y , Nil)))) = do
  v <- eval y
  (e:es) <- get
  put $ (Map.insert x (Expr v) e):es
  return v
--defunc
eval (Pair (Symbol 0xDEF22C, Pair (Symbol s, Pair (x, Pair (y, Nil))))) = do
  let f = listToPairs [Symbol 0xF22C, x, y]
  (e:es) <- get
  put $ (Map.insert s (Expr f) e):es
  return f
--exec func
eval (Pair (Symbol f, x)) = do
  env <- get
  (param, prog) <- case (lookupEnv f env) of
    Just (Expr (Pair (Symbol 0xF22C, Pair (param, Pair (prog, Nil))))) -> return (param, prog)
    _ -> lift $ error "error invalid func"
  ex <- mapM eval $ pairsToList x
  let arg = List.map Expr ex
  let prm = List.map symbolToInteger $ pairsToList param
  funcEnv <- if List.length prm == List.length arg
             then return $ fromList $ List.zip prm arg
             else lift $ error "invalid arg"
  put $ funcEnv:env
  v <- eval prog
  modify List.tail
  return v
  where
  symbolToInteger (Symbol s) = s
  symbolToInteger _ = undefined
--symbol
eval (Symbol x) = do
  env <- get
  case (lookupEnv x env) of
    Just (Expr v) -> return v
    _ -> lift $ error $ "error invalid symbol " ++ show (Symbol x)
--otherwise
eval expr = do
  lift $ error $ "error: " ++ show expr
  return Nil

--TODO lambda read

keywordEnv = fromList $ do
{
  [
    (0x77, Expr $ Symbol 0x77),
    envTuple 0xCA70 1,
    envTuple 0xCD70 1,
    envTuple 0xA703 1,
    envTuple 0x9207E0 1,
    envTuple 0x3717E0 1,
    envTuple 0xBE9120 1,
    envTuple 0xC025 2,
    envTuple 0xE9 2,
    envTuple 0xADD0 2,
    envTuple 0x52B0 2,
    envTuple 0x5E79 2,
    envTuple 0x1F 3,
    envTuple 0xDEF22C 3
  ]
}
  where
  envTuple s n = (s, genFunc s n)
  genFunc s n = Expr $ listToPairs $ [Symbol 0xF22C, listToPairs $ List.map Symbol [1..n], listToPairs $ List.map Symbol (s:[1..n])]

repl env = do
  IO.putStr "hexsp> "
  b <- IO.getLine
  let bb = Text.unpack $ Text.concat $ List.intersperse (Text.pack " ") $ chunksOf 2 $ Text.map Char.toUpper $ Text.filter (/= ' ') $ Text.pack b
  if List.null bb then repl env else return ()
  let e = parse (many expr) "" $ bb ++ " "
  case e of
    Right x -> repl.snd =<< f x env
    Left y -> print y >> repl env
  where
  f [] env = print "parse error" >> return (Nil, env)
  f (x:[]) env = do
    (a, s) <- runStateT (eval x) env
    print a
    return (a, s)
  f (x:xs) env = do
    (a, s) <- runStateT (eval x) env
    print a
    f xs s

main = do
  args <- getArgs
  if List.null args then repl [keywordEnv] else return ()
  a <- BS.readFile $ List.head args
  let b = List.concat $ List.map (\i -> printf "%02X " i) $ BS.unpack $ a :: String

  let e = parse (many expr) "" b
  case e of
    Right x -> f x [keywordEnv] >> return ()
    Left y -> print y
  where
  f (x:[]) s = runStateT (eval x) s
  f (x:xs) s = (f xs) =<< execStateT (eval x) s
