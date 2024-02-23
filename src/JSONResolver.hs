module JSONResolver (
    parse,
) where

import qualified Data.Char as C
import qualified Data.Functor.Identity as ID
import GHC.Base (Alternative ((<|>)))
import qualified Text.Parsec as P

data Number' = Int Int | Float Float deriving (Show)
data JSON
    = Null
    | Number Number'
    | String String
    | Bool Bool
    | Undefined
    | Object [(String, JSON)]
    | List [JSON]
    deriving (Show)

parse :: String -> Either P.ParseError JSON
parse text = P.parse jsonParsec "JSON:" text

jsonParsec :: P.Parsec String () JSON
jsonParsec = P.spaces *> myParsec <* P.spaces <* P.eof

myParsec :: P.ParsecT String () ID.Identity JSON
myParsec =
    nullParsec
        <|> stringParsecM
        <|> stringParsecD
        <|> listParsec
        <|> objectParsec
        <|> boolParsec
        <|> undefinedParsec

--        <|> floatParsec
--        <|> digitParsec

undefinedParsec :: P.ParsecT String u ID.Identity JSON
undefinedParsec = Undefined <$ P.string "undefined"
nullParsec :: P.ParsecT String u ID.Identity JSON
nullParsec = P.string "null" >> return Null

boolParsec :: P.ParsecT String u ID.Identity JSON
boolParsec = (Bool True <$ P.string "true") <|> (Bool False <$ P.string "false")

stringParsecM :: P.ParsecT String u ID.Identity JSON
stringParsecM = do
    _ <- P.oneOf "\'"
    x <- P.many $ P.noneOf "\'"
    _ <- P.oneOf "\'"
    return $ String x

stringParsecD :: P.ParsecT String u ID.Identity JSON
stringParsecD = do
    _ <- P.oneOf "\""
    x <- P.many $ P.noneOf "\""
    _ <- P.oneOf "\""
    return $ String x

listParsec :: P.ParsecT String () ID.Identity JSON
listParsec = do
    _ <- P.char '['
    P.spaces
    a <- P.sepBy myParsec (P.try symbol1)
    P.spaces
    _ <- P.char ']'
    return $ List a

symbol1 :: P.ParsecT String u ID.Identity ()
symbol1 = do
    P.spaces
    _ <- P.char ','
    P.spaces

keyParsec :: P.Parsec String () String
keyParsec = do
    c <- P.lookAhead P.anyChar
    let val
            | C.isDigit c = fail "Invalid key."
            | otherwise = P.many1 $ P.noneOf ": "
    val
objectInnerParsec :: P.ParsecT String () ID.Identity (String, JSON)
objectInnerParsec = do
    (String key) <- stringParsecM <|> stringParsecD <|> (pure String <*> keyParsec) P.<?> "Valid key."
    P.spaces
    _ <- P.char ':'
    P.spaces
    val <- myParsec
    return (key, val)

objectParsec :: P.ParsecT String () ID.Identity JSON
objectParsec = do
    _ <- P.char '{'
    P.spaces
    a <- P.sepBy objectInnerParsec (P.try symbol1)
    P.spaces
    _ <- P.char '}'
    return $ Object a