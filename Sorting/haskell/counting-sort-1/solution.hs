-- import Data.Array
-- import qualified Data.ByteString.Char8 as BS
-- import Data.Maybe

-- sort :: [Int] -> [(Int, Int)]
-- sort =  assocs . accumArray (+) 0 (0,99) . flip zip (repeat 1)

-- parse :: BS.ByteString -> [Int]
-- parse = fmap (fst . fromJust . BS.readInt) . tail . BS.words

-- main = BS.interact $ BS.unwords . fmap (BS.pack . show . snd) . sort . parse






import Control.Monad
import Control.Applicative
import Data.List
import Data.Ord
import Data.Char
import Data.Maybe
import Data.Array
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C

main = do
            getLine
            s<-map read.words <$> getContents::IO [Int]
            let arra= accumArray (+) 0 (0,99) (zip s (repeat 1))
            putStrLn $ intercalate " " $ map show $ elems arra
