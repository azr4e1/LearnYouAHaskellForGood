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
--
-- Neat little demonstration of how currying works: the following two are equivalent:
addThree :: (Num a) => a -> a -> a -> a
addThree x y z = x + y + z

addThree' :: (Num a) => a -> a -> a -> a
addThree' = \x -> \y -> \z -> x + y + z

-- Folds
-- A fold takes a binary function, a starting value (I like to call it the accumulator) and a list to fold up. The binary function itself takes two parameters. The binary function is called with the accumulator and the first (or last) element and produces a new accumulator. Then, the binary function is called again with the new accumulator and the now new first (or last) element, and so on. Once we've walked over the whole list, only the accumulator remains, which is what we've reduced the list to.
sum' :: (Num a) => [a] -> a
sum' xs = foldl (\acc x -> acc + x) 0 xs

elem' :: (Eq a) => a -> [a] -> Bool
elem' y ys = foldl (\acc x -> if x == y then True else acc) False ys
-- The right fold, foldr works in a similar way to the left fold, only the accumulator eats up the values from the right. Also, the left fold's binary function has the accumulator as the first parameter and the current value as the second one (so \acc x -> ...), the right fold's binary function has the current value as the first parameter and the accumulator as the second one (so \x acc -> ...)

map'' :: (a -> b) -> [a] -> [b]
map'' f xs = foldr (\x acc -> f x : acc) [] xs

-- Right folds work on infinite lists, whereas left ones don't
-- Folds can be used to implement any function where you traverse a list once, element by element, and then return something based on that. Whenever you want to traverse a list to return something, chances are you want a fold.
-- The foldl1 and foldr1 functions work much like foldl and foldr, only you don't need to provide them with an explicit starting value. They assume the first (or last) element of the list to be the starting value and then start the fold with the element next to it. With that in mind, the sum function can be implemented like so: sum = foldl1 (+).

-- scanl and scanr are like foldl and foldr, only they report all the intermediate accumulator states in the form of a list. There are also scanl1 and scanr1, which are analogous to foldl1 and foldr1.
d = scanl (+) 0 [3, 5, 2, 1]
-- When using a scanl, the final result will be in the last element of the resulting list while a scanr will place the result in the head.
sqrtSums :: Int
sqrtSums = length (takeWhile (<1000) (scanl1 (+) (map sqrt [1..])))+1
-- we use takewhile because filter doesn't work on infinite lists
--
-- Function application with $
-- ($) :: (a -> b) -> a -> b
-- f $ x = f x
-- What is the difference between this function application and the normal function application with space? While normal function application has really high precedence, the $ function has the lower precedence. Function application with a space is left-associative (f a b c  =((f a) b) c), function application with $ is right-associative
-- THis is just a convenient function so that we don't have to write so many parentheses. We could write
e = sum (map sqrt [1..130])
-- like
f = sum $ map sqrt [1..130]
-- Another example
g = sum (filter (> 10) (map (*2) [2..10]))
h = sum $ filter (>10) $ map (*2) [2..10]

-- Besides this, $ operator means we can treat function application as any other function; that way we can for instance map function application over a list of functions
i :: [Float]
i = map ($ 3) [(4+), (10*), (^2), sqrt]

-- Function composition
-- Haskell's function composition is exactly the same as mathematical function composition.
-- the operator is .
--
-- (.) :: (b -> c) -> (a -> b) -> a -> c
-- f . g = \x -> f $ g x
-- One of the uses for function composition is making functions on the fly to pass to other functions. Sure, can use lambdas for that, but many times, function composition is clearer and more concise. Say we have a list of numbers and we want to turn them all into negative numbers. One way to do that would be to get each number's absolute value and then negate it, like so:
-- map (\x -> negate (abs x)) [5,-3,-6,7,-3,2,-19,24]
-- but with function composition:
-- map (negate . abs) [5,-3,-6,7,-3,2,-19,24]
-- function composition is right associative:
-- map (\xs -> negate (sum (tail xs)))
-- is equivalent to
-- map (negate . sum . tail)
--
-- For functions that take several paramteres, we have to partially apply them just so much that each function takes just one parameter
-- For example, sum (replicate 5 (max 6.7 8.9)) can be rewritten as sum . replicate 5 . max 6.7 $ 8.9
-- If you want to rewrite an expression with a lot of parentheses by using function composition, you can start by putting the last parameter of the innermost function after a $ and then just composing all the other function calls, writing them without their last parameter and putting dots between them.
-- Another common use of function composition is defining functions in the so-called point-free style (pointless style)
sum'' :: (Num a) => [a] -> a
sum'' xs = foldl (+) 0 xs
-- because of currying, we can omit the xs on both sides: sum'' = foldl (+) 0; this is called writing in point free style
-- how do we write the following in point free style?
fn x = ceiling (negate (tan (cos (max 50 x))))
-- we need function composition:
fn' = ceiling . negate . tan . cos . max 50
-- Many times, a point free style is more readable and concise, because it makes you think about functions and what kind of functions composing them results in instead of thinking about data and how it's shuffled around. You can take simple functions and use composition as glue to form more complex functions. However, many times, writing a function in point free style can be less readable if a function is too complex. That's why making long chains of function composition is discouraged
-- The preferred style is to use let bindings to give labels to intermediary results or split the problem into sub-problems and then put it together so that the function makes sense to someone reading it instead of just making a huge composition chain.
-- For example, this is how to solve a problem in three different styles:
oddSquareSum :: Integer
oddSquareSum = sum (takeWhile (<1000) (filter odd (map (^2) [1..])))
-- with function composition:
oddSquareSum' :: Integer
oddSquareSum' = sum . takeWhile (<1000) . filter odd . map (^2) $ [1..]
-- with let bindings (and preferred way)
oddSquareSum'' :: Integer
oddSquareSum'' =
    let oddSquares = filter odd $ map (^2) [1..]
        belowLimit = takeWhile (<1000) oddSquares
    in sum belowLimit

