import Nbc
import Math
import Data.Conduit
import System.IO
import qualified Data.Conduit.Binary as CB
import Control.Monad.Trans.Resource
import System.Environment
import Data.Maybe
import Control.Monad.IO.Class (liftIO)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Conduit.List as CL
import Data.List.Split (splitOn)
import qualified Data.Map as M
import Data.List
import System.Random
import Control.Monad.State
import Control.Parallel.Strategies
import Control.DeepSeq

source :: FilePath -> Source (ResourceT IO) String
source fp = do
    bracketP
        (openFile fp ReadMode)
        (hClose)
        readByLines        
    where
        readByLines handle = do
            eof <- liftIO $ hIsEOF handle
            if eof
                then return ()
                else do
                    l <- liftIO $ hGetLine handle                    
                    yield l
                    readByLines handle

consoleOutSink :: (Show a) => Sink a (ResourceT IO) ()
consoleOutSink = CL.mapM_ (\x -> liftIO $ print x)

fileOutSink :: (Show a) => FilePath -> Sink a (ResourceT IO) ()
fileOutSink fp = do
    bracketP
        (openFile fp WriteMode)
        (hClose)
        writeByLines
    where
        writeByLines handle = awaitForever $ liftIO . hPrint handle
-----------------------------------------------------------------
readMaybe :: Read a => String -> Maybe a
readMaybe st = case reads st of
                [(x, "")] -> Just x
                _ -> Nothing

cutOff :: Bool -> ([a] -> [a]) -> [a] -> [a]
cutOff p f xs
    | p == True = f xs
    | otherwise = xs

parseOnMapMaybe :: Bool -> [String] -> (String, Maybe Vector)
parseOnMapMaybe ifc xs = (last xs, sequence . map (\x -> readMaybe x :: Maybe Double) . ignoreFirstColumn $ init xs)
    where
        ignoreFirstColumn = cutOff ifc init

parseConduit :: String -> Bool -> Conduit String (ResourceT IO) (String, Vector)
parseConduit fs ifc = awaitForever $ yield . parseClass . parseOnMapMaybe ifc . splitOn fs    

getOutSink :: Show a => Maybe FilePath -> Sink a (ResourceT IO) ()
getOutSink fp = case fp of
                Just f -> fileOutSink f
                Nothing -> consoleOutSink

parseClass :: (String, Maybe a) -> (String, a)
parseClass (c, vs) = case vs of
                        Nothing -> error "Incorrect data"
                        Just v -> (c, v)

getFaultsPercent :: [(String, Vector)] -> ClassifyResult -> Double
getFaultsPercent xs cs = faultsCount / totalCount
    where
        faultsCount = foldl (\acc (a,a1) -> if a /= a1 then acc + 1 else acc) 0 
                        . zipWith (\x y -> (fst x, fst y)) xs $ cs
        totalCount = fromIntegral . length $ xs

randomlyDivide :: StdGen -> Double -> [a] -> ([a], [a])
randomlyDivide g p xs = (\(xs, ys) -> (map fst xs, map fst ys)) . partition (\x -> snd x <= p) $ zippWithMask
    where
        zippWithMask = zip xs rms
        rms = take (length xs) $ randomRs (0, 1) g

type ClassifyData = [(String, Vector)]
type TrainingDataPercent = Double
type FaultsPercent = Double
type RetryCount = Int
type TrainingSetSpec = M.Map String [(Double, Double)]

data ClassifierSpec = ClassifierSpec {
    trainingSetSpec :: TrainingSetSpec,
    trainingSetIndexes :: [Int]
}

instance Show ClassifierSpec where
    show a = trainingSetSpecString ++ "\n" ++ trainingSetIndexesString
        where
            trainingSetSpecString = foldl (\acc x -> show x ++ "\n" ++ acc ) "" $ M.toList . trainingSetSpec $ a
            trainingSetIndexesString = show $ trainingSetIndexes a

getTrainingSetSpec :: TrainingSet -> TrainingSetSpec 
getTrainingSetSpec = M.map (map (\xs -> (mean xs, dispersion xs)) . transpose)                     

getTrainingSetIndexes :: ClassifyData -> TrainingSet -> [Int]
getTrainingSetIndexes cd ts = map (indexOf cdList) tsList
    where 
        tsList = concat . map snd . M.toList $ ts
        cdList = map snd cd

indexOf :: Eq a => [a] -> a -> Int
indexOf x xs = case (elemIndex xs x) of
                Just i -> i
                Nothing -> error "something went wrong"

type ClassifyValue = (ClassifyResult, TrainingSet)
type ClassifyState = (Double, ClassifyValue)

getBestClassifier :: ClassifyData -> TrainingDataPercent -> RetryCount -> StdGen 
                    -> State ClassifyState ClassifyValue
getBestClassifier _ _ 0 _ = do
        (_, result) <- get
        return result

getBestClassifier cd p n g = do
    (fp, result) <- get
    
    if (nfp < fp) then
        put (nfp, classifyTotalResult)
    else
        put (fp, result)

    getBestClassifier cd p (n - 1) ng 

    where
        (trainingData, testData) = randomlyDivide g p cd
        ng = snd $ next g
        trainingSet = M.fromList 
            . map (foldl (\(_,a1) (b,b1) -> (b, b1 : a1)) ("", []))
            . groupBy (\(a,_) (a1,_) -> a == a1) 
            . sort
            $ trainingData

        testSet = map snd testData
        classifyResult = classify trainingSet testSet
        nfp = getFaultsPercent testData classifyResult
        classifyTotalResult = (classifyResult, trainingSet)

-----------------------------------------------------------------
main = do

    args <- getArgs
    let inFile = args !! 0
    let accuracy = readMaybe $ args !! 1 :: Maybe Double
    let retryCount = read $ args !! 2 :: Int
    
    let accuracyParsed = case (accuracy) of
                        Just acc -> acc
                        Nothing -> error "Incorrect accuracy"

    parsedMapList <- runResourceT $ source inFile $$ (parseConduit "," False) =$ (CL.consume)
    
    gens <- sequence $  genericReplicate retryCount newStdGen
    
    let states = parMap rpar (force $ getBestClassifier parsedMapList accuracyParsed 1) gens
    
    let classifiers = map (flip runState (1, ([], M.empty))) states
    let (bestClassifier, _) = minimumBy (\(a,(fp, v)) (a1,(fp1, v1)) -> compare fp fp1) classifiers
    let (classifyResult, trainingSet) = bestClassifier

    let classifierSpec = ClassifierSpec {
        trainingSetSpec = getTrainingSetSpec trainingSet,
        trainingSetIndexes = getTrainingSetIndexes parsedMapList trainingSet
    }  

    runResourceT $ CL.sourceList classifyResult $$ getOutSink (Nothing)
    runResourceT $ CL.sourceList ([classifierSpec]) $$ getOutSink (Just "bestClassifierSpec.txt")