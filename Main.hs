import Nbc
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

consoleOutSink :: Sink (String, Double) (ResourceT IO) ()
consoleOutSink = CL.mapM_ (\x -> liftIO $ print x)

fileOutSink :: FilePath -> Sink (String, Double) (ResourceT IO) ()
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

getOutSink :: Maybe FilePath -> Sink (String, Double) (ResourceT IO) ()
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
                                Nothing -> error "Incorrect training data"
                                Just v -> (c, v)
-----------------------------------------------------------------
main = do

    args <- getArgs
    let inFile = args !! 0

    parsedMapList <- runResourceT $ source inFile $$ (parseConduit "," False) =$ (CL.consume)

    g <- getStdGen
    let (trainingData, testData) = randomlyDivide g 0.8 parsedMapList

    let trainingSet = M.fromList 
            . map parseClass
            . map (foldl (\(_,a1) (b,b1) -> (b, (:) <$> b1 <*> a1)) ("", Just []))
            . groupBy (\(a,_) (a1,_) -> a == a1) 
            . sort 
            $ trainingData

    let testSet = map parseClass testData

    putStrLn "Training set: "
    print trainingSet

    putStrLn "Test set: "
    print testSet
    putStrLn "-------------------------------"

    let classifyResult = classify trainingSet $ map snd testSet

    runResourceT $ CL.sourceList classifyResult $$ getOutSink (Nothing)