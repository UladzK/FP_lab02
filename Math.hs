module Math
(
    mean,
    dispersion,
    Vector
) where

type Vector = [Double]

mean :: Vector -> Double
mean xs = sum xs / (fromIntegral $ length xs)

dispersion :: Vector -> Double
dispersion xs = 1 / (n - 1) * foldl (\acc x -> (x - xsm)**2 + acc) 0 xs
    where
        n = fromIntegral $ length xs
        xsm = mean xs