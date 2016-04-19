{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}

module Carga where

import Control.Lens
import qualified Data.Map as M

import Import

data Disc = Disc { _prof :: Text, _nome :: Text }
    deriving (Ord, Eq, Show)

data Hor = Hor { _dia :: Int, _hora :: Int }
    deriving (Ord, Eq, Show)

data Turno = Diurno | Noturno
    deriving (Ord, Eq, Show)

numberTurno :: Int -> Turno
numberTurno n | n == 1 = Diurno
              | n == 2 = Noturno
              | otherwise = error "Turno Invalido"

data Quadro = Quadro [(Hor, Int, Disc)]
    deriving Show

type Precolor = (Int, Hor, Disc)

data CargaJ = CargaJ { __fases :: [(Int, Turno)]
                    , __profs :: [(Disc, Int)]
                    , __precolors :: [Precolor] }
data Carga = Carga { _fases :: Map Int Turno
                   , _profs :: [(Disc, Int)]
                   , _precolors :: [Precolor] }
    deriving Show

$(makeLenses ''Disc)
$(makeLenses ''Hor)
$(makeLenses ''Carga)
$(makeLenses ''Quadro)



toCarga :: CargaJ -> Carga
toCarga c =  Carga { _fases = M.fromList $ __fases c
                   , _profs = __profs c
                   , _precolors = __precolors c }


fromCarga :: Carga -> CargaJ
fromCarga c = CargaJ { __fases = M.toList $ _fases c
                     , __profs = _profs c
                     , __precolors = _precolors c }

emptyCarga :: Carga
emptyCarga = Carga M.empty [] []

dias :: [Int]
dias = [1..5]

horsTurno :: Turno -> [Hor]
horsTurno Diurno  = [Hor d h | d <- dias, h <- [7, 10]]
horsTurno Noturno = [Hor d h | d <- dias, h <- [19, 21]]


horariosFases :: M.Map Int Turno -> [(Int, Hor)]
horariosFases fm = concat [[(c, h) | h <- horsTurno t] | (c, t) <- M.toList fm]

manhaSeguinte :: Hor -> Hor -> Bool
manhaSeguinte (Hor d1 h1) (Hor d2 h2) = d2 == d1 + 1 && h1 == 21 && h2 == 7

mesmoDia :: Hor -> Hor -> Bool
mesmoDia (Hor d1 h1) (Hor d2 h2) | d1 /= d2  = False
                                 | otherwise = if h1 <= 12
                                                  then h2 <= 12
                                                  else h2 > 12

consecutivos :: Hor -> Hor -> Bool
consecutivos (Hor d1 h1) (Hor d2 h2) = d1 == d2 && ((h1 == 7 && h2 == 10) || (h1 == 19 && h2 == 21))

addProf :: Int -> Disc -> Carga -> Carga
addProf f p c = profs %~ ((p, f):) $ c

addProf2 :: Int -> Disc -> Carga -> Carga
addProf2 f p c = addProf f p $ addProf f p c

addTurno :: Int -> Turno -> Carga -> Carga
addTurno f t c = fases %~ M.insert f t $ c

addPrecolor :: Precolor -> Carga -> Carga
addPrecolor  p c = precolors %~ (p:)  $ c

