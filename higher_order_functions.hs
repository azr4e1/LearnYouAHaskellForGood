-- Haskell functions can take functions as parameters and return functions as return values. A function that does that is called higher order function.
--
-- Every function in Haskell officially takes only one parameter. All the functions that accept several parameters are actually **curried functions**. Let's see an example:
-- max 4 5; what it does is it creates a function that takes a parameter and returns either 4 or that parameter, depending on which one is bigger; then, 5 is applied to that function. The following are equivalent:
-- max 4 5
-- (max 4) 5
--
-- Putting a space between two things is _function application_. Space is sort of an operator with highest precedence.
-- if we call a function with too few parameters, we get back a partially applied function, meaning a function that takes as many parameters as we left out. Using partial application (calling functions with too few parameters, if you will) is a neat way to create functions on the fly so we can pass them to another function or to seed them with some data.
multThree :: (Num a) => a -> (a -> (a -> a))
multThree x y z = x * y * z

multTwoWithNine = multThree 9

multTwoWithEighteen = multTwoWithNine 2

compareWithHundred :: (Num a, Ord a) => a -> Ordering
compareWithHundred = compare 100

-- Infix functions can also be partially applied by using sections. To section an infix function, simply surround it with parentheses and only supply a parameter on one side. That creates a function that takes one parameter and then applies it to the side that's missing an operand.
divideByTen :: (Floating a) => a -> a
divideByTen = (/ 10)

-- Calling, say, divideByTen 200 is equivalent to doing 200 / 10, as is doing (/10) 200
isUpperAlphanum :: Char -> Bool
isUpperAlphanum = (`elem` ['A' .. 'Z'])

-- functions can take functions as arguments and return them as values
applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

-- Before, we didn't need parentheses because -> is naturally right-associative. However, here, they're mandatory. They indicate that the first parameter is a function that takes something and returns that same thing. The second parameter is something of that type also and the return value is also of the same type.
-- ghci> applyTwice (+3) 10  
-- 16  
-- ghci> applyTwice (++ " HAHA") "HEY"  
-- "HEY HAHA HAHA"  
-- ghci> applyTwice ("HAHA " ++) "HEY"  
-- "HAHA HAHA HEY"  
-- ghci> applyTwice (multThree 2 2) 9  
-- 144  
-- ghci> applyTwice (3:) [1]  
-- [3,3,1]

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _  _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

-- zipWith' (+) [4, 2, 5, 6] [2, 6, 2, 3]
-- zipWith' max [6,3,2,1] [7,3,1,5]
-- zipWith' (++) ["foo ", "bar ", "baz "] ["fighters", "hoppers", "aldrin"]

flip' :: ( a -> b -> c ) -> (b -> a -> c)
flip' f = g
    where g x y = f y x

-- Taking advantage of the fact that functions are curried:
flip'' :: (a -> b -> c) -> b -> a -> c
flip'' f y x = f x y

-- Maps and filters
-- map takes a function and a list and applies that function to every element in the list, producing a new list.
map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x:map' f xs

-- filter is a function that takes a predicate (a predicate is a function that returns a boolean) and a list and then returns the list of elements that satisfy the predicate
filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x:xs)
    | p x = x:filter' p xs
    | otherwise = filter' p xs


quickSort :: (Ord a) => [a] -> [a]
quickSort [] = []
quickSort (x:xs) = 
    let smallerSorted = quickSort (filter (<=x) xs)
        biggerSorted = quickSort (filter (>x) xs)
    in smallerSorted ++ [x] ++ biggerSorted

largestDivisible :: (Integral a) => a
largestDivisible = head (filter p [100000, 99999..])
    where p x = x `mod` 3829 == 0

largestDivisible' :: (Integral a) => a
largestDivisible' = last (take ((100000 `div` 3829)+1) [0, 3829..])

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' p (x:xs)
    | p x = x:takeWhile' p xs
    | otherwise = []

a = sum (takeWhile' (<10000) (filter odd (map (^2) [1..])))

-- Collatz sequence
chain :: (Integral a) => a -> [a]
chain 1 = [1]
chain n
    | even n = n:chain (n `div` 2)
    | odd n = n:chain (n*3 + 1)

-- For all starting numbers between 1 and 100, how many chains have a length greater than 15?
-- chain 10
-- chain 30
numLongChains' = length (filter (>15) (map length (map chain [1..100])))

numLongChains :: Int
numLongChains = length (filter isLong (map chain [1..100]))
    where isLong xs = length xs > 15


-- Interesting application of curried functions:
listOfFuns = map (*) [0..]
b = (listOfFuns !! 4) 5

-- Lambdas
-- These are basically anonymous functions. We make lambdas with the sole purpose of passing them to higher-order functions.
-- To make a lambda, we write a \ (because it kind of looks like the greek letter lambda if you squint hard enough) and then we write the parameters, separated by spaces. After that comes a -> and then the function body. We usually surround them by parentheses, because otherwise they extend all the way to the right.
numLongChains'' :: Int
numLongChains'' = length (filter (\xs -> length xs > 15) (map chain [1..100]))

-- Lambdas are expressions that return a function
-- Lambdas can take any number of parameters
c = zipWith (\a b -> (a*30+3) / b) [5, 4, 3, 2, 1] [1, 2, 3, 4, 5]

-- You can also pattern match in lambdas; however, you can't define several patterns for one parameter; if a pattern matching fails in a lambda, a runtime error occurs
-- map (\(a, b) -> a+b) [(1,2),(3,5),(6,3),(2,6),(2,5)]
