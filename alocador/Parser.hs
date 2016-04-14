module Parser where

import Model
import Control.Lens ((.~), (%~))
import Text.ParserCombinators.Parsec

csvFile :: GenParser Char st Carga
csvFile = 
    do results <-  many line
       eof
       return $ foldl addResult emptyCarga results

data ResLine = P (Integer, Disc) | T (Integer, Turno) | PC (Integer, Hor, Disc) | Comment
    deriving Show

addResult :: Carga -> ResLine -> Carga
addResult c (P (i, p)) = addProf2 i p c 
addResult c (T (i, t)) = addTurno i t c
addResult c (PC x ) = addPrecolor x c
addResult c Comment = c

-- Each line contains 1 or more cells, separated by a comma
line :: GenParser Char st ResLine
line = 
    do 
       c <- ( parseProf <|> parseFase <|> parseComment <|> parsePrecolor )
       eol                       -- end of line
       return c
       

quotedString :: GenParser Char st String
quotedString = do
    char '"'
    x <- (many $ noneOf "\"")
    char '"'
    many (oneOf " ")
    return x

number :: GenParser Char st Integer 
number = do
    x <- many (oneOf ['0'..'9'])
    many (oneOf " ")
    return (read x :: Integer)

identifier :: String -> GenParser Char st String 
identifier s = do
    r <- string s
    many (oneOf " ")
    return r

parseComment :: GenParser Char st ResLine
parseComment = do
    string "--"
    many (noneOf "\n")
    return Comment

parsePrecolor :: GenParser Char st ResLine
parsePrecolor = do
    identifier "precolor"
    n <- number
    dia <- number
    h <- number
    p <- quotedString
    d <- quotedString
    return $ PC (n, Hor dia h, Disc p d)

parseProf :: GenParser Char st ResLine
parseProf = 
    do
       identifier "disc"
       n <- number
       p <- quotedString
       d <- quotedString
       return $ P (n, Disc p d)

parseTurno :: GenParser Char st Turno
parseTurno =
    do 
       s <- string "Diurno" <|> string "Noturno"
       many (oneOf " ")
       case s of
           "Diurno" -> return Diurno
           "Noturno" -> return Noturno

parseFase :: GenParser Char st ResLine
parseFase =
    do 
       identifier "fase"
       n <- number
       t <- parseTurno
       return $ T (n, t)


{-parseHor :: GenParser Char st Carga
parseHor = do
    identifier "hor"
    n <- number 
    s <- quotedString
    return $ hor %~ (Hor n (Just s):) $ emptyCarga

precolor :: GenParser Char st Carga
precolor = do
    identifier "color"
    number 
    number
    return emptyCarga

parseRestr :: GenParser Char st Carga
parseRestr = do
    identifier "restr"
    n1 <- number
    n2 <- number
    return $ restr %~ (Restr (Hor n1 Nothing, Hor n2 Nothing) :) $ emptyCarga
-}


-- The end of line character is \n
eol :: GenParser Char st Char
eol = char '\n'

parseCSV :: String -> Either ParseError Carga
parseCSV input = parse csvFile "(unknown)" input
