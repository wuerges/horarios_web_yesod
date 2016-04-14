{-# LANGUAGE OverloadedStrings #-} 

import Model
import View
import Parser
import Algo
import System.Environment
import Control.Applicative
import Data.Either
import Data.Aeson
import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy.Encoding as T




main = 
    do args <- getArgs
       case args of 
            ("--text":[]) -> mainText
            --("--json":[]) -> mainJson
            _ -> mainJson

doCarga r = 
    case solve r of
        Left e -> T.putStrLn $ prettyPrintErr e
        Right s -> T.putStrLn $ prettyPrint s

mainText = do
    parse <- parseCSV <$> getContents
    case parse of 
               Left e -> putStrLn $ "Parse error: \n" ++ show e
               Right r -> doCarga r

mainJson = do
    cts <- T.getContents
    let parse = eitherDecodeCargaJ $ T.encodeUtf8 cts
    case parse of 
               Right [r] -> doCarga (toCarga r)
               Right [] -> doCarga emptyCarga
               Left e -> putStrLn $ "Parse error: \n" ++ show e


eitherDecodeCargaJ s = (eitherDecode s :: Either String [CargaJ])
