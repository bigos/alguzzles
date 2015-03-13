-- Enter your code here. Read input from STDIN. Print output to STDOUT
import Data.List
import Data.Char

noncontig xs mnm = (sum.(\u-> if (u==[]) then [mnm] else u).(filter (>0))) xs

contig [] acc s = s
contig (a:xs) acc s = if (acc+a>0) then (contig xs (acc+a) (max s (acc+a))) else (contig xs 0 s)

react :: [[Integer]] -> [[Integer]]
react [] = []
react (n:xs:ls) = let mnm = maximum xs in ([contig xs 0 mnm,noncontig xs mnm]:(react ls))

main = interact (unlines.(map (unwords.(map show))).react.(map ((map (\u->read u::Integer)).words)).tail.lines)
