module Algo where

import Data.Graph.Inductive
import Model
import Control.Lens
import Data.Maybe
import Data.Either
import Data.List
import qualified Data.Map as H
import qualified Data.Set as S


uniq :: Ord a => [a] -> [a]
uniq = S.toList . S.fromList

type G = Gr (Integer, Hor) ()

type PM = H.Map Int Disc
type TODO = [(Disc, [Int])]

breakP p = case break (=='&') p of
    (p1, "") -> [p1]
    (p1, _:ps) -> p1:(breakP ps)

profProib :: String -> String -> Bool
profProib p1s p2s = any (\(a, b) -> a == b) [(p1, p2) | p1 <- breakP p1s, p2 <- breakP p2s]
        
neighs :: PM -> G -> Int -> [(Disc, Hor)]
neighs pm g n = catMaybes $ map (\n1 -> res (H.lookup n1 pm, lab g n1)) $ neighbors g n
    where res (Just x, Just (f, h)) = Just (x, h)
          res _ = Nothing

color1n :: PM -> G -> Disc -> Int -> Maybe PM
color1n pm g c n = 
    if H.notMember n pm 
    then 
        if any (proib c) (neighs pm g n) 
        then Nothing
        else Just $ H.insert n c pm
    else Nothing
  where proib (Disc p1 d1) (Disc p2 d2, h2) = 
            if consecutivos h1 h2 || consecutivos h2 h1
            then d1 == d2
            else profProib p1 p2
        h1 = snd $ fromJust $ lab g n

color1ns :: PM -> G -> Disc -> [Int] -> Maybe PM
color1ns pm g c = 
    listToMaybe . catMaybes . map (color1n pm g c)

-- Colors a whole TODO or fails

bestTodo :: [(TODO, PM)] -> (TODO, PM)
bestTodo ts = head (sortOn fst $ take 1000 ts)

colorPermute :: PM -> G -> TODO -> (TODO, PM)
colorPermute pm g todo = bestTodo $ map (color pm g) (permutations todo)

color :: PM -> G -> TODO -> (TODO, PM)
color pm g [] = ([], pm)
color pm g todo@((c, ns):cs) = case color1ns pm g c ns of 
    Just pm' -> color pm' g cs
    Nothing -> (todo, pm)

color' :: TODO -> PM -> G -> [TODO] -> (TODO, PM)
color' errs pm g [] = (errs, pm)
color' errs pm g (t:ts) = case colorPermute pm g t of 
    ([], pm') -> color' errs pm' g ts
    (e', pm') -> color' (e' ++ errs) pm' g ts
    
genNodes :: Carga -> [LNode (Integer, Hor)]
genNodes c = zip [1..] (horariosFases $ c ^. fases)

genTodoProf ns (p, f1) = (p, [n | (n, (f2, _)) <- ns, f1 == f2])

-- permutations $ map concat $ sequenceA $ map permutations [[1, 2], [4]]

groupFases :: [(Disc, Integer)] -> [[(Disc, Integer)]]
groupFases = groupBy (\a b -> snd a == snd b) . sortOn snd

-- permuteGroups :: [[(Disc, Integer)]] -> [[(Disc, Integer)]]
-- permuteGroups gs = map concat $ sequenceA $ map permutations $ gs

genTodoProfs :: [LNode (Integer, Hor)] -> [(Disc, Integer)] -> TODO
genTodoProfs ns ps = map (genTodoProf ns) ps

genTodos' ns ps = map (genTodoProfs ns) (groupFases $  ps)

genTodos :: Carga -> [LNode (Integer, Hor)] -> [TODO]
genTodos c ns = genTodos' ns ( c ^. profs )

genTodo :: Carga -> [LNode (Integer, Hor)] -> TODO
genTodo c ns = genTodoProfs ns ( c ^. profs )

allEdges ns = [(e1, e2) | e1 <- ns, e2 <- ns]
filterEdge (a@(n1, (f1, h1)), b@(n2, (f2, h2))) = 
        (h1 == h2) || manhaSeguinte h1 h2 || (f1 == f2 && consecutivos h1 h2)

makeQuadro :: PM -> G -> Quadro
makeQuadro pm g = Quadro [(h n g, f n g, p) | (n, p) <- H.toList pm]
    where h n g = snd $ fromJust $ lab g n :: Hor
          f n g = fst $ fromJust $ lab g n :: Integer

precolorMap :: Carga -> [LNode (Integer, Hor)] -> PM
precolorMap c ns = H.fromList [(n, d) | 
                                (n, (f1, h1)) <- ns, 
                                (f2, h2, d) <- c ^. precolors,
                                (f1 == f2) && (h1 == h2)
                              ]

solve :: Carga -> Either ([Disc], Quadro) Quadro
solve c = q
    where ns = genNodes c
          es = filter filterEdge (allEdges ns)
          es' = map (\((n1, _), (n2, _)) -> (n1, n2, ())) es
          g = mkGraph ns es' :: G
          tds = genTodos c ns
          pcm = precolorMap c ns 
          q = case color' [] pcm g tds of
            ([], pm') -> Right $ makeQuadro pm' g
            (td, pm') -> Left (map fst td, makeQuadro pm' g)
