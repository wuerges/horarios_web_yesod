{-# LANGUAGE TemplateHaskell #-}

module Model where

import Control.Lens
import Text.ParserCombinators.Parsec
import Data.Maybe
import qualified Data.Map as H
import Data.Set as S
import Data.Aeson.TH
import Data.Aeson

data Disc = Disc { _prof :: String, _nome :: String }
    deriving (Ord, Eq, Show)

data Hor = Hor { _dia :: Integer, _hora :: Integer }
    deriving (Ord, Eq, Show)

data Turno = Diurno | Noturno
    deriving (Ord, Eq, Show)
    
type Precolor = (Integer, Hor, Disc)

--type Fase = (Integer, 

data CargaJ = CargaJ { __fases :: [(Integer, Turno)]
                    , __profs :: [(Disc, Integer)]
                    , __precolors :: [Precolor] }
data Carga = Carga { _fases :: H.Map Integer Turno
                   , _profs :: [(Disc, Integer)] 
                   , _precolors :: [Precolor] }
    deriving Show 


toCarga :: CargaJ -> Carga
toCarga c =  Carga { _fases = H.fromList $ __fases c
                   , _profs = __profs c
                   , _precolors = __precolors c }


fromCarga :: Carga -> CargaJ
fromCarga c = CargaJ { __fases = H.toList $ _fases c
                     , __profs = _profs c
                     , __precolors = _precolors c }

emptyCarga = Carga H.empty [] []

data Quadro = Quadro [(Hor, Integer, Disc)]
    deriving Show 

$(makeLenses ''Disc)
$(makeLenses ''Hor)
$(makeLenses ''Carga)
$(makeLenses ''Quadro)

dias = [1..5]
horsTurno Diurno  = [Hor d h | d <- dias, h <- [7, 10]]
horsTurno Noturno = [Hor d h | d <- dias, h <- [19, 21]]

horariosFases fm = concat [[(c, h) | h <- horsTurno t] | (c, t) <- H.toList fm]

manhaSeguinte (Hor d1 h1) (Hor d2 h2) = d2 == d1 + 1 && h1 == 21 && h2 == 7

consecutivos (Hor d1 h1) (Hor d2 h2) = d1 == d2 && ((h1 == 7 && h2 == 10) || (h1 == 19 && h2 == 21))

addProf :: Integer -> Disc -> Carga -> Carga
addProf f p c = profs %~ ((p, f):) $ c

addProf2 :: Integer -> Disc -> Carga -> Carga
addProf2 f p c = addProf f p $ addProf f p c

addTurno :: Integer -> Turno -> Carga -> Carga
addTurno f t c = fases %~ H.insert f t $ c

addPrecolor :: Precolor -> Carga -> Carga
addPrecolor  p c = precolors %~ (p:)  $ c


deriveJSON defaultOptions ''Hor
deriveJSON defaultOptions ''Disc
deriveJSON defaultOptions ''Quadro
deriveJSON defaultOptions ''Turno
deriveJSON defaultOptions ''CargaJ
