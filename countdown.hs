data Value = Int
data Op = Add | Sub | Mul | Div
data Expr = Num Int | App Op Expr Expr

apply Add = (+)
apply Sub = (-)
apply Mul = (*)
apply Div = (/)

subseqs [x] = [[x]]
subseqs (x : xs) = xss ++ [x] : map (x :) xss
                   where xss = subseqs xs

value :: Expr -> Value
value (Num x) = x
value (App op e1 e2) = apply op (value e1) (value e2)

legal :: Op -> Value -> Value -> Bool
legal Add v1 v2 = True
legal Sub v1 v2 = (v2 < v1)
legal Mul v1 v2 = True
legal Div v1 v2 = (v1 mod v2 == 0)

mkExprs :: [Value] -> [(Expr, Value)]
mkExprs [x] = [(Num x, x)]
mkExprs xs = [ev | (ys, zs) <- unmerges xs,
                   ev1 <- mkExprs ys,
                   ev2 <- mkExprs zs,
                   ev <- combine ev1 ev2]

unmerges :: [a] -> [([a], [a])]
unmerges [x, y]   = [([x], [y]), ([y], [x])]
unmerges (x : xs) = [([x], xs), (xs, [x])] ++
                    concatMap (add x) (unmerges xs)
                    where add x (ys, zs) = [(x : ys, zs), (ys, x : zs)]

combine :: (Expr, Value) -> (Expr, Value) -> [(Expr, Value)]
combine (e1, v1) (e2, v2) = [(App op e1 e2, apply op v1 v2) | op <- ops, legal op v1 v2]
                            where ops = [Add, Sub, Mul, Div]

nearest n ((e, v) : evs) = if d == 0 then (e, v)
                           else search n d (e, v) evs
                           where d = abs (n - v)

search n d ev [] = ev
search n d ev ((e, v) : evs) | d' == 0  = (e, v)
                             | d' < d   = search n d' (e, v) evs
                             | d' >= d  = search n d ev evs
                               where d' = abs (n - v)

countdown1 :: Int -> [Int] -> (Expr, Value)
countdown1 n = nearest n . concatMap mkExprs . subseqs 

