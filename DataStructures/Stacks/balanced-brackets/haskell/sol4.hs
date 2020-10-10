main = interact $ unlines . map (solve []) . tail . lines

solve [] [] = "YES"
solve a [] = "NO"
solve a (b:s) | b == '(' || b == '{' || b == '[' = solve (b : a) s
solve ('(':a) (')':s) = solve a s
solve ('{':a) ('}':s) = solve a s
solve ('[':a) (']':s) = solve a s
solve _ _ = "NO"
