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
        writeByLines handle = do
            s <- await
            case s of
                Nothing -> return ()
                Just str -> do
                    liftIO $ hPrint handle str
                    writeByLines handle
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

parseConduit :: String -> Bool -> Conduit String (ResourceT IO) (String, Maybe Vector)
parseConduit fs ifc = do
    s <- await
    case s of
        Nothing -> return ()
        Just str -> do      
            yield $ parseOnMapMaybe ifc . splitOn fs $ str
            parseConduit fs ifc

getOutSink :: Show a => Maybe FilePath -> Sink a (ResourceT IO) ()
getOutSink fp = case fp of
                Just f -> fileOutSink f
                Nothing -> consoleOutSink

randomlyDivide :: StdGen -> Double -> [a] -> ([a], [a])
randomlyDivide g p xs = (\(xs, ys) -> (map fst xs, map fst ys)) . partition (\x -> snd x <= p) $ zippWithMask
    where
        zippWithMask = zip xs rms
        rms = take (length xs) $ randomRs (0, 1) g

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

type TrainingSetSpec = M.Map String [(Double, Double)]

getTrainingSetSpec :: TrainingSet -> TrainingSetSpec 
getTrainingSetSpec = M.map (map (\xs -> (mean xs, dispersion xs)) . transpose)                     

--getTrainingSetIndexes :: ClassifyData -> TrainingSet -> [Int]
--getTrainingSetIndexes cd ts = 

type ClassifyData = [(String, Maybe Vector)]
type TrainingDataPercent = Double
type FaultsPercent = Double
type RetryCount = Int
type ClassifyValue = (ClassifyResult, TrainingSet)
type ClassifyState = (Double, ClassifyValue)

getBestClassifier :: StdGen -> ClassifyData -> TrainingDataPercent -> RetryCount 
                    -> State ClassifyState ClassifyValue

getBestClassifier _ _ _ 0 = do
        (_, result) <- get
        return result

getBestClassifier g cd p n = do
    (fp, result) <- get
    if (nfp < fp) then
        put (nfp, classifyTotalResult)        
    else
        put (fp, result)
    getBestClassifier ng cd p (n - 1)

    where
        (trainingData, testData) = randomlyDivide g p cd
        ng = snd $ next g
        trainingSet = M.fromList 
            . map parseClass
            . map (foldl (\(_,a1) (b,b1) -> (b, (:) <$> b1 <*> a1)) ("", Just []))
            . groupBy (\(a,_) (a1,_) -> a == a1) 
            . sort 
            $ trainingData

        testSet = map parseClass testData
        classifyResult = classify trainingSet $ map snd testSet
        nfp = getFaultsPercent testSet classifyResult
        classifyTotalResult = (classifyResult, trainingSet)

-----------------------------------------------------------------
main = do

    args <- getArgs
    let inFile = args !! 0

    parsedMapList <- runResourceT $ source inFile $$ (parseConduit "," False) =$ (CL.consume)

    g <- getStdGen
    
    let (classifyResult, trainingSet) = evalState (getBestClassifier g parsedMapList 0.8 3) (1, ([], M.empty))
    
    let trainingSetSpec = getTrainingSetSpec trainingSet
    
    runResourceT $ CL.sourceList (M.toList trainingSetSpec) $$ getOutSink (Just "bestClassifierSpec.txt")
    runResourceT $ CL.sourceList classifyResult $$ getOutSink (Nothing)