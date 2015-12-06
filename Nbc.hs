import qualified Data.Map as M

type Vector = [Double]
type TrainingSet = M.Map String [Vector]
type ClassifyResult = M.Map Vector String

mean :: Vector -> Double
mean xs = sum xs / (fromIntegral $ length xs)

dispersion :: Double -> [Vector] -> Double
dispersion xi xs = 1 / (n - 1) * foldl (\acc x -> (xi - mean x)**2 + acc) 0 xs
    where 
        n = fromIntegral $ length xs

attrLikelihood :: [Vector] -> Vector -> Double -> Double
attrLikelihood vs xs xi = 1 / (sqrt sigmaSqrDoubled * pi) * exp ( (xi - mean xs)^2 / sigmaSqrDoubled * (-1))
    where 
        sigmaSqrDoubled = 2 * dispersion xi vs

likelihood :: [Vector] -> Vector -> Vector -> Double
likelihood vs ys xs =  product . map (attrLikelihood vs ys) $ xs
    where
        aprioriLikelihood = 

classify :: [Vector] -> [Vector] -> [Double]
classify ts xs = map (\x -> maximum . map (\y -> likelihood ts y ) ts ) xs
