import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad
import Numeric
import Data.Ratio
import Data.Complex

{--=============================================================================================--
    An Algebraic data type, defining a set of possible values that a variable of type LispVal
    can hold:
    1. An ATOM, which stores a String naming the atom
    2. A LIST, which stores a list of other LispVal (Haskell lists are denoted by brackets); 
       also called a proper list
    3. A DOTTEDLIST, representing the Scheme form (a b . c); also called an improper list. 
       This stores a list of all elelments but the last, and then stores the last element as 
       another field
    4. A NUMBER, containing a Haskell Integer
    5. A STRING, containing a Haskell String
    6. A BOOL, containing a Haskell boolean value
--=============================================================================================--}
data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | Float Double
             | Ratio Rational
             | Complex (Complex Double)
             | String String
             | Bool Bool

{--=============================================================================================--
    Parser
--=============================================================================================--}

parseString :: Parser LispVal
parseString = do 
    char '"'
    x <- many $ escapeChars <|> noneOf "\"\\"
    char '"'
    return $ String x -- $ is used to avoid parentheses: identical to return (String x) -- "apply"

escapeChars :: Parser Char
escapeChars = do 
    char '\\' -- a backslash
    x <- oneOf "\\\"nrt" -- backslash, doublequote, n, r, t
    return $ case x of
        '\\' -> x
        '"'  -> x
        'n'  -> '\n'
        'r'  -> '\r'
        't'  -> '\t'

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

parseAtom :: Parser LispVal
parseAtom = do
    first <- letter <|> symbol
    rest <- many (letter <|> digit <|> symbol)
    let atom = first : rest
    return $ case atom of
        "#t" -> Bool True
        "#f" -> Bool False
        _    -> Atom atom

parseBool :: Parser LispVal
parseBool = do
    char '#'
    (char 't' >> return (Bool True)) <|> (char 'f' >> return (Bool False))

parseFloat :: Parser LispVal
parseFloat = do
    x <- many1 digit
    char '.'
    y <- many1 digit
    return $ Float (fst . head $ readFloat (x ++ "." ++ y))

parseRatio :: Parser LispVal
parseRatio = do
    x <- many1 digit
    char '/'
    y <- many1 digit
    return $ Ratio ((read x) % (read y))

toDouble :: LispVal -> Double
toDouble(Float f) = realToFrac f
toDouble(Number n) = fromIntegral n

parseComplex :: Parser LispVal
parseComplex = do
    x <- (try parseFloat <|> parseDecimal1)
    char '+'
    y <- (try parseFloat <|> parseDecimal1)
    char 'i'
    return $ Complex (toDouble x :+ toDouble y)

parseNumber :: Parser LispVal
parseNumber = parseDecimal1
    <|> parseDecimal2
    <|> parseOct 
    <|> parseHex 
    <|> parseBin

parseDecimal1 :: Parser LispVal
parseDecimal1 = many1 digit >>= (return . Number . read)

parseDecimal2 :: Parser LispVal
parseDecimal2 = do 
    try $ string "#d"
    x <- many1 digit
    (return . Number . read) x

parseHex :: Parser LispVal
parseHex = do
    try $ string "#x"
    x <- many1 hexDigit
    return $ Number $ hex2dig x

parseOct :: Parser LispVal
parseOct = do
    try $ string "#o"
    x <- many1 octDigit
    return $ Number $ oct2dig x

parseBin :: Parser LispVal
parseBin = do
    try $ string "#b"
    x <- many1 $ oneOf "10"
    return $ Number $ bin2dig x

oct2dig x = fst $ readOct x !! 0
hex2dig x = fst $ readHex x !! 0
bin2dig = bin2dig' 0
bin2dig' digint "" = digint
bin2dig' digint (x:xs) = let old = 2 * digint + (if x == '0' then 0 else 1)
    in bin2dig' old xs

parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
    head <- endBy parseExpr spaces
    tail <- char '.' >> spaces >> parseExpr
    return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = do
    char '\''
    x <- parseExpr
    return $ List [Atom "quote", x]

parseExpr :: Parser LispVal
parseExpr = parseAtom
        <|> parseString
        <|> try parseComplex <|> try parseFloat <|> try parseRatio <|> parseNumber
        <|> parseBool
        <|> parseQuoted
        <|> do 
                char '('
                x <- try parseList <|> parseDottedList
                char ')'
                return x

readExpr :: String -> LispVal
readExpr input = case parse parseExpr "" input of
    Left err -> String $ "No match : " ++ show err
    Right val -> val

spaces :: Parser ()
spaces = skipMany1 space

{--=============================================================================================--
    Beginning the Evaluator
--=============================================================================================--}

showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Float contents) = show contents
showVal (Ratio contents) = show contents
showVal (Complex contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

instance Show LispVal where show = showVal

eval :: LispVal -> LispVal
eval val@(String _) = val
eval val@(Number _) = val
eval val@(Bool _) = val
eval (List [Atom "quote", val]) = val
eval (List (Atom func : args)) = apply func $ map eval args

apply :: String -> [LispVal] -> LispVal
apply func args = maybe (Bool False) ($ args) $ lookup func primitives

primitives :: [(String, [LispVal] -> LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem)]

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinop op params = Number $ foldl1 op $ map unpackNum params

unpackNum :: LispVal -> Integer
unpackNum (Number n) = n
unpackNum (String n) = let parsed = reads n :: [(Integer, String)] in
    if null parsed then 0 else fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n
unpackNum _ = 0

main :: IO ()
main = getArgs >>= print . eval . readExpr . head
