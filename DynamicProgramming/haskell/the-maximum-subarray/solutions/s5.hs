main = do
	t<-readLn :: IO Int
	loop t

loop 0 = return ()
loop t = do
	l<-readLn :: IO Int
	arr<-fmap ((map (\x -> read x::Int)) . words) getLine
	let a = show (msc arr)
	let b = show (msnc arr)
	putStr (a++" "++b++"\n")
	loop $t-1

msc l = if al == [] then maximum l else maximum al
	where (al,_) = foldl (\(q,w) x -> if w+x <0 then (q,0) else ((w+x):q,w+x)) ([],0) (l)

msnc l =  if al == [] then maximum l else sum al
    where al = (filter (>=0) l)
