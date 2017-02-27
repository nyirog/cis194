fib :: Integer -> Integer
fib 0 = 1
fib 1 = 1
fib n = (fib (n-1)) + (fib (n-2))

fibs1 :: [Integer]
fibs1 = fib <$> [1..]

fibs2 :: [Integer]
fibs2 = 1:1:zipWith (+) fibs2 (tail fibs2)


data Stream a = Cons a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Cons x xs) = x:streamToList xs

instance Show a => Show (Stream a) where
  show xs = show (take 20 (streamToList xs)) ++ "..."

streamRepeat :: a -> Stream a
streamRepeat x = Cons x (streamRepeat x)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons x xs) = Cons (f x) (streamMap f xs)

streamIterate :: (a -> a) -> a -> Stream a
streamIterate f x = Cons x (streamIterate f (f x))

streamInterleave :: Stream a -> Stream a -> Stream a
streamInterleave (Cons x xs) ys = Cons x (streamInterleave ys xs)

nats :: Stream Integer
nats = streamIterate (+1) 0

ruler :: Stream Integer
ruler = streamInterleave (streamRepeat 0) (streamMap (+1) ruler)


data Supply s a = S (Stream s -> (a, Stream s))


get :: Supply s s
get = S (\(Cons x xs) -> (x, xs))

pureSupply :: a -> Supply s a
pureSupply x = S(\xs -> (x, xs))


mapSupply :: (a -> b) -> Supply s a -> Supply s b
mapSupply f (S t) = S go 
  where go xs = let (a, xs') = t xs
                in  (f a, xs')

mapSupply2 :: (a -> b -> c) -> Supply s a -> Supply s b -> Supply s c
mapSupply2 f (S t') (S t'') = S go
  where go xs = let (a, xs') = t' xs
                    (b, xs'') = t'' xs'
                in (f a b, xs'')

bindSupply :: Supply s a -> (a -> Supply s b) -> Supply s b
bindSupply (S t) f = S go
  where go xs = let (a, xs') = t xs
                    (S t') = f a
                    (b, xs'') = t' xs'
                in (b, xs'')

runSupply :: Stream s -> Supply s a -> a
runSupply s (S t) = fst (t s)


instance Functor (Supply s) where
  fmap = mapSupply

instance Applicative (Supply s) where
  pure = pureSupply
  (<*>) = mapSupply2 id

instance Monad (Supply s) where
  return = pureSupply
  (>>=) = bindSupply


data Tree a = Node (Tree a) (Tree a) | Leaf a deriving Show

labelTree :: Tree a -> Tree Integer
labelTree t = runSupply nats (go t)
  where
    go :: Tree a -> Supply s (Tree s)
    go (Node t1 t2) = Node <$> go t1 <*> go t2
    go (Leaf _) = Leaf <$> get
