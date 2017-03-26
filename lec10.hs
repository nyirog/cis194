import Test.QuickCheck


data Stream a = Cons a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Cons x xs) = x : streamToList xs

instance Show a => Show (Stream a) where
    show s = init (show (take 20 (streamToList s))) ++ "…"

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

-- Exercise 3

data Supply s a = S (Stream s -> (a, Stream s))

get :: Supply s s
get = S (\(Cons x xs) -> (x, xs))

pureSupply :: a -> Supply s a
pureSupply x = S (\xs -> (x, xs))

mapSupply :: (a -> b) -> Supply s a -> Supply s b
mapSupply f (S t) = S go
  where go xs = let (a, xs') = t xs
                in  (f a, xs')

mapSupply2 :: (a -> b -> c) -> Supply s a -> Supply s b -> Supply s c
mapSupply2 f (S t1) (S t2) = S go
  where go xs = let (a, xs')  = t1 xs
                    (b, xs'') = t2 xs'
                in  (f a b, xs'')

bindSupply :: Supply s a -> (a -> Supply s b) -> Supply s b
bindSupply (S t1) k = S go
  where go xs = let (a, xs')  = t1 xs
                    (S t2)    = k a
                    (b, xs'') = t2 xs'
                in  (b, xs'')

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


instance Arbitrary a => Arbitrary (Tree a) where
    arbitrary = genTree


genTree :: Arbitrary a => Gen (Tree a)
genTree = do
        stop_now <- arbitrary
        if stop_now
            then do
                leaf <- arbitrary
                return $ Leaf leaf
            else do
                left <- genTree
                right <- genTree
                return $ Node left right


size :: Tree a -> Int
size t = go 0 t where
    go :: Int -> Tree  a -> Int
    go acc (Leaf leaf) = acc + 1
    go acc (Node left right) = acc + (go 0 left) + (go 0 right)


toList :: Tree a -> [a]
toList t = go [] t where
    go acc (Leaf leaf) = leaf:acc
    go acc (Node left right) = acc ++ (go [] left) ++ (go [] right)


prop_lengthToList :: Tree Integer -> Bool
prop_lengthToList t = length (toList t) == size t

prop_sizeLabelTree :: Tree Integer -> Bool
prop_sizeLabelTree t = size t == size (labelTree t)

prop_labelTree :: Tree Integer -> Bool
prop_labelTree t = toList (labelTree t) == [0 .. go] where
    go = toInteger ((size t) - 1)

prop_labelTreeIdempotent :: Tree Integer -> Bool
prop_labelTreeIdempotent t = toList (labelTree t) == toList (labelTree (labelTree t))

main = do
    quickCheck prop_lengthToList
    quickCheck prop_sizeLabelTree
    quickCheck prop_labelTree
    quickCheck prop_labelTreeIdempotent
