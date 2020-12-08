module Main where

import Data.Array
import Data.List
import System.Environment
import System.IO

main :: IO()
main = do
    stdout <- getEnv "OUTPUT_PATH"
    fptr <- openFile stdout WriteMode

    n <- readLn :: IO Integer
    arrTemp <- getLine
    let result1 = elems $ accumArray (+) 0 (0,n-1) (map (\x->(x,1)) (map read (words arrTemp) :: [Integer]))
    let result = unwords $ map show result1
    hPutStrLn fptr result
    hFlush fptr
    hClose fptr

-- import Data.Array
-- import qualified Data.ByteString.Char8 as BS
-- import Data.Maybe

-- sort :: [Int] -> [(Int, Int)]
-- sort =  assocs . accumArray (+) 0 (0,99) . flip zip (repeat 1)

-- parse :: BS.ByteString -> [Int]
-- parse = fmap (fst . fromJust . BS.readInt) . tail . BS.words

-- main = BS.interact $ BS.unwords . fmap (BS.pack . show . snd) . sort . parse






-- import Control.Monad
-- import Control.Applicative
-- import Data.List
-- import Data.Ord
-- import Data.Char
-- import Data.Maybe
-- import Data.Array
-- import qualified Data.ByteString as B
-- import qualified Data.ByteString.Char8 as C

-- main = do
--             getLine
--             s<-map read.words <$> getContents::IO [Int]
--             let arra= accumArray (+) 0 (0,99) (zip s (repeat 1))
--             putStrLn $ intercalate " " $ map show $ elems arra
