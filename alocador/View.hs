{-# LANGUAGE OverloadedStrings #-} 
module View where

import Model
import Control.Lens

import Data.Aeson
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy.Encoding as T


toCsv :: [T.Text] -> T.Text
toCsv = T.intercalate ", "

prettyPrintErr :: ([Disc], Quadro) -> T.Text
prettyPrintErr (d, q) = T.decodeUtf8 . encode $ ("Failure"::T.Text, d, q)

prettyPrint :: Quadro -> T.Text
prettyPrint q =  T.decodeUtf8 . encode $ ("Success"::T.Text, []::[Disc], q)
