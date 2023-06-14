-- -- Haskell has a static type system; the type of every expression is known at compile time.
-- -- Haskell has type inference, it can infer the type from the object.
-- --
-- -- A type is a kind of label that every expression has. It tells in which category of things the expression belongs.
-- -- The `:t` command in GHCI tells us the type. `:l` loads a module
-- :t 'a'
-- :t True
-- :t "HELLO!"
-- :t (True, 'a')
-- :t 4 == 5
-- :: reads as `has type of`. Explicit types are denoted with the first letter in capital case
-- Functions also have type; we can choose to give them explicit type declaration
removeNonUppercase :: [Char] -> [Char] -- explicit type declaration
removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z']]

-- Multiple parameters
addThree :: Int -> Int -> Int -> Int
addThree x y z = x + y + z
-- No special distinction between the parameters and the return type
-- Some common types:
-- Int -> integers; it is bounded (has min and max)
-- Integer -> also integers; it's not bounded so it can be used to represent really big numbers
factorial :: Integer -> Integer
factorial n = product [1..n]

-- Float is real floating point with single precision
-- circumference :: Float -> Float
-- circumference r = 2*pi*r
-- circumference 4.0
-- Double is a real floating point with double the precision
circumference :: Double -> Double
circumference r = 2*pi*r
-- Bool is a boolean type: True or False
-- Char represents a character; denoted by single quotes
-- Tuples are types but they are dependent on their length as well as the types of their components.
-- The empty tuple () is also a type which can only have a single value: ()
-- Type variables: if you test :t head, the type of this function is `[a] -> a`; all types are in capital case, so it's not a variable; it's actually a `type variable`, which means that `a` can be of any type. This allows to write very generic functions.
-- Functions that have type variables are called `polymorphic functions` (can be of multiple types).
-- A typeclass can be represented as a math function that to each type defined assigns lawful operations on that type. It is a sort of interface that defines some behaviour
-- If we do :t (==), we see
-- (==) :: Eq a => a -> a -> Bool
-- Everything before => is called a class constraint; it means that the type variables must belong to the class contraint specified.
-- The Eq typeclass provides an interface for testing equality. Anyt type where is makes sense to test for equality between two values should be a member of the Eq typeclass.
-- Some basic typeclass instances:
-- Eq: types that support equality testing
-- Ord is for types that have an ordering
-- Show is a typeclass instance whose members can be presened as strings
-- show 3
-- Read is the opposite typeclass of Show; the `read` function takes a string and returns a type which is a member of Read
read "True" && False
read "8.3" + 3.9
read "5" - 2
read "[1, 2, 3, 4]" ++ [3]
-- Just returning read throws error because Haskell doesn't know what tyep return out of the Read typeclass. We can circumvent this with explicit type annotation
read "5" :: Int
read "5" :: Float
(read "5" :: Float) * 4
read "[1, 2, 3, 4]" :: [Int]
-- Enum members are sequentially ordered types - they can be enumerated. We can use them in list ranges. They have successor and predecessor, that you can obtain with `succ` and `pred`. Types in this class include: (), Bool, Char, Ordering, Int, Integer, Float and Double
-- Bounded members have an upper and a lower bound
-- Num is a numeric typeclass. Its memebers have the property of being able to act like numbers
