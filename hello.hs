import Test.QuickCheck
-- multiplication of 2 numbers
f x y = x * y

-- fib function
fib 0 = 0
fib 1 = 1
fib 2 = 1
fib n | n > 2 = fib(n-1)+fib(n-2)

absolute x | x >= 0 = x
           | otherwise = -x

-- or
--absolute x | x >= 0 = x    |   absolute x = if x >= 0 then x else -x
--aboslute x | x < 0 = -x    |

-- take the power of a number
power x 0 = 1
power x n | n > 0 = x * power x(n-1)

-- or power x n = if n == 0 then 1 else x * power x (n-1)


