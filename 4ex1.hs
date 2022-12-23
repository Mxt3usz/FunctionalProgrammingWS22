import Test.QuickCheck

ones :: [Integer]
ones = 1 : ones

repeat' :: a -> [a]
repeat' x = x : repeat' x

repeat'' :: a -> [a]
repeat'' x = xs
  where xs = x : xs

nfib :: Integer -> Integer
nfib n | n == 0 = 0
       | n == 1 = 1
       | otherwise = nfib (n-1) + nfib (n-2)

nfib1000 = nfib 30

fib :: [Integer]
fib = 0 : 1 : zipWith (+) fib (tail fib)
  --- fib (n+2) = fib (n+1) + fib n
  --- fib_0 = zipWith (+) fib_1 fib_2

badfib = 1 : zipWith (+) badfib (tail badfib)

primes :: [Integer]
primes = sieve [2..]

sieve (p : xs) = p : sieve (filter (\x -> x `mod` p /= 0) xs)




-- Exercise Sheet 4 Task 1

-- a)
iterate' f x = x : iterate' f (f x)

--b)
cycle' xs = f
  where f = xs ++ f

-- Quiz : Does foldl or foldr or both or netiher evaluate for infintie lists

-- -> answer only foldr because in foldl we want to compare last element with
-- the backwards recusrive applications of the function f which is not possible because
-- infinite list has no last element
-- -> foldr compares first element with the forward recursive applications of the function f
-- here we only need to know the first element which is always defined and also
-- because of forward application we can always terminate

foldr' f nilr [] = nilr
foldr' f nilr (x:xs) =  x `f` (foldr' f nilr xs)

-- take 10 $ foldr' (:) [] [0..] -> [0,1,2,3,4,5,6,7,8,9]



data Vector2D = Vector2D{a :: Double,b :: Double}



instance Show Vector2D where 
  show (Vector2D a b) = "(" ++ show a ++ " \n " ++ show b ++ ")"

instance Eq Vector2D where
  Vector2D a b == Vector2D a' b' = a == a' && b == b'
    
instance Num Vector2D where
  Vector2D a b + Vector2D a' b' = Vector2D (a + a') (b + b')
  Vector2D a b - Vector2D a' b' = Vector2D (a - a') (b - b')
  Vector2D a b * Vector2D a' b' = Vector2D (a * a') (b * b')
  abs(Vector2D a b) = Vector2D (abs a) (abs b)
  signum(Vector2D a b) | a == 0 && b == 0 = 0
                       | a < 0 || b < 0 = -1
                       | otherwise = 1
  fromInteger i = Vector2D (fromInteger i) (fromInteger i)
h = Vector2D 3 1
l = Vector2D 3 2

data Sum = Sum{e :: Integer}
  deriving (Show,Eq)

data Product = Product{c :: Integer}
  deriving Show

instance Semigroup Sum where
  (<>) (Sum a) (Sum a') = Sum (a + a')

instance Monoid Sum where
    mempty = Sum 0

prop_assoc a'' b'' c'' = (Sum a'') <> ((Sum b'') <> (Sum c'')) == (Sum b'') <> ((Sum a'') <> (Sum c''))

prop_left_iden a''' = mempty <> Sum a''' == Sum a'''

prop_right_iden a'''' = Sum a'''' <> mempty == Sum a''''
  
instance Semigroup Product where
  (<>) (Product a) (Product a') = Product (a * a')

instance Monoid Product where
    mempty = Product 1


n = Sum 4

p = Sum 5

foldMap'' f' [] = mempty
foldMap'' f' (x:xs) = f' x <> foldMap'' f' xs


mapsum = (\ x -> Sum x )
mapprod = (\ x -> Product x )