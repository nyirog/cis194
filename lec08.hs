{-# LANGUAGE InstanceSigs #-}

data ComplicatedA a b
    = Con1 a b
    | Con2 [Maybe (a -> b)]

instance Functor (ComplicatedA c) where
    fmap :: (a -> b) -> ComplicatedA c a -> ComplicatedA c b
    fmap f (Con1 c a) = Con1 c (f a)
    fmap f (Con2 gs) = Con2 (map go gs) where
        go (Just g) = Just (f . g)
        go Nothing = Nothing

data ComplicatedB f g a b
    = Con3 (f a)
    | Con4 (g b)
    | Con5 (g (g [b]))

instance Functor g => Functor (ComplicatedB f g a) where
    fmap :: (a -> b) -> ComplicatedB f g c a -> ComplicatedB f g c b
    fmap _ (Con3 fa) = Con3 fa
    fmap t (Con4 gb) = Con4 (fmap t gb)
    fmap t (Con5 ggbs) = Con5 (fmap (fmap (map t)) ggbs)

func0 :: Monad f => (a -> a) -> f a -> f a
func0 f xs = do
    x <- xs
    return (f (f x))

func0' :: Functor f => (a -> a) -> f a -> f a
func0' f xs = (f . f) <$> xs

func1 :: Monad f => f a -> f (a,a)
func1 xs = xs >>= (\x -> return (x,x))

func1' :: Functor f => f a -> f (a, a)
func1' xs = (\x -> (x, x)) <$> xs

func2 :: Monad f => f a -> f (a,a)
func2 xs = xs >>= (\x -> xs >>= \y -> return (x,y))

func2' :: Applicative f => f a -> f (a, a)
func2' xs = (\x  y -> (x, y)) <$> xs <*> xs

func3 :: Monad f => f a -> f (a,a)
func3 xs = xs >>= (\x -> xs >>= \y -> return (x,x))

func3' :: Applicative f => f a -> f (a, a)
func3' xs = (\x  y -> (x, x)) <$> xs <*> xs

func4 :: Monad f => f a -> f a -> f (a,a)
func4 xs ys = xs >>= (\x -> ys >>= \y -> return (x,y))

func4' :: Applicative f => f a -> f a -> f (a, a)
func4' xs ys = (\x y -> (x, y)) <$> xs <*> ys

func5 :: Monad f => f Integer -> f Integer -> f Integer
func5 xs ys = do
    x <- xs
    let x' = x + 1
    y <- (+1) <$> ys
    return (x' + y)

func5' :: Applicative f => f Integer -> f Integer -> f Integer
func5' xs ys = (\x y -> (x + 1 + y + 1)) <$> xs <*> ys

func6 :: Monad f => f Integer -> f (Integer,Integer)
func6 xs = do
    x <- xs
    return $ if x > 0 then (x, 0)
                      else (0, x)

func6' :: Functor f => f Integer -> f (Integer,Integer)
func6' xs = (\x -> if x > 0 then (x, 0) else (0, x)) <$> xs

func7 :: Monad f => f Integer -> f (Integer,Integer)
func7 xs = do
    x <- xs
    if x > 0 then return (x, 0)
             else return (0, x)

func8 :: Monad f => f Integer -> Integer -> f Integer
func8 xs x = pure (+) <*> xs <*> pure x

func8' :: Functor f => f Integer -> Integer -> f Integer
func8' xs x = (\x' -> x + x') <$> xs

func9 :: Monad f => f Integer -> f Integer -> f Integer -> f Integer
func9 xs ys zs = xs >>= \x -> if even x then ys else zs

func10 :: Monad f => f Integer -> f Integer
func10 xs = do
    x <- xs >>= (\x -> return (x * x))
    return (x + 10)

func10' :: Functor f => f Integer -> f Integer
func10' xs = (\x -> (x*x)+10) <$> xs


data Parser a  = P (String -> Maybe (a, String))

runParser :: Parser t -> String -> Maybe (t, String)
runParser (P p) = p

parse :: Parser a -> String -> Maybe a
parse p s = case runParser p s of
    Just (r, "") -> Just r
    _ -> Nothing

noParser :: Parser a
noParser = P (\_ -> Nothing)

pureParser :: a -> Parser a
pureParser a = P go where
    go "" = Just (a, "")
    go _ = Nothing


instance Functor Parser where
    fmap :: (a -> b) -> Parser a -> Parser b
    fmap f p = P p' where
        p' s = case runParser p s of
            Just (r, s') -> Just (f r, s')
            Nothing -> Nothing


instance Applicative Parser where
    pure = pureParser
    fp <*> fx = P $ \s -> case runParser fp s of
        Just (f, s') -> case runParser fx s' of
            Just (r, s'') -> Just (f r, s'')
            Nothing -> Nothing
        Nothing -> Nothing

instance Monad Parser where
    return = pureParser
    (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    fa >>= k = P $ \s -> case runParser fa s of
        Just (r, s') -> runParser (k r) s'
        Nothing -> Nothing

anyChar :: Parser Char
anyChar = P go where
    go [c] = Just (c, [])
    go _ = Nothing

main :: IO ()
main = print "egg"
