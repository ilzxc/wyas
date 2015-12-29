import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad
import Numeric

{--=============================================================================================--
    An Algebraic data type, defining a set of possible values that a variable of type LispVal
    can hold:
    1. An ATOM, which stores a String naming the atom
    2. A LIST, which stores a list of other LispVals (Haskell lists are denoted by brackets); 
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
             | String String
             | Bool Bool

{--=============================================================================================--
    parseString uses the do-notation instead of the >> operator. The reason for this is that
    we need to retrieve the value of our parse (returned by many (noneOf "\"")) and manipulating
    it.
    In general:
        * use >> if the actions don't return a value;
        * use >>= if the value will be immediately passed into the next action;
        * do-notation otherwise.
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

{- 
    read the parseNumber right-to-left:
        * many1 digit grab all the digits
        * read to use the Haskell parser
        * Number the result to cast to a Number
    However, the result of the many1 digit is still a Parser String, so we
    can not feed it directly into (Number . read) function composition. The
    standard function LiftM tells the function to operate on the value inside
    the monad, giving us back a Parser LispVal (REQUIRES Control.Monad)
-}
{- uncomment for the liftM version: -}
--parseNumber :: Parser LispVal
--parseNumber = liftM (Number . read) $ many1 digit

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
        <|> parseNumber
        <|> parseBool
        <|> parseQuoted
        <|> do 
                char '('
                x <- try parseList <|> parseDottedList
                char ')'
                return x

readExpr :: String -> String
readExpr input = case parse parseExpr "" input of
    Left err -> "No match : " ++ show err
    Right val -> "Found value"

spaces :: Parser ()
spaces = skipMany1 space

main :: IO ()
main = do
    (expr:_) <- getArgs
    putStrLn (readExpr expr)

