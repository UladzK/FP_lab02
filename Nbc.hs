module Nbc
(
    Vector,
    TrainingSet,
    ClassifyResult,
    classify    
) where

import qualified Data.Map as M
import Data.List
import Control.Applicative

type Vector = [Double]
type TrainingSet = M.Map String [Vector]
type ClassifyResult = [(String, Double)]

cmpTuplesByVal :: Ord b => (a, b) -> (a, b) -> Ordering
cmpTuplesByVal (a0, b0) (a1, b1)
    | b0 > b1 = GT
    | b0 < b1 = LT
    | otherwise = EQ

mean :: Vector -> Double
mean xs = sum xs / (fromIntegral $ length xs)

dispersion :: Vector -> Double
dispersion xs = 1 / (n - 1) * foldl (\acc x -> (x - xsm)**2 + acc) 0 xs
    where
        n = fromIntegral $ length xs
        xsm = mean xs

attrLikelihood :: Vector -> Double -> Double
attrLikelihood vs xi = 1 / (sqrt sigmaSqrDoubled * pi) * exp ( (xi - mean vs)^2 / sigmaSqrDoubled * (-1))
    where
        sigmaSqrDoubled = 2 * dispersion vs

likelihood :: [[Vector]] -> Vector -> [Vector] -> Double
likelihood ts xs vs = aprioriLikelihood * product attributesLikelihoods
    where
        aprioriLikelihood = classAttributesCount / totalAttributesCount
        classAttributesCount = fromIntegral $ length vs
        totalAttributesCount = fromIntegral $ foldl (\a b -> length b + a) 0 ts        
        classAttributes = transpose vs
        attributesLikelihoods = zipWith attrLikelihood classAttributes xs        

classify :: TrainingSet -> [Vector] -> ClassifyResult
classify ts xs = map (\x -> maximumBy cmpTuplesByVal $ M.toList (M.map (likelihood totalSet x) ts) ) xs
    where
        totalSet = M.elems ts