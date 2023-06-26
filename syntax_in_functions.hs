-- Pattern Matching
-- consists of specifying pattern which some data should conform to; we can define separate function bodies for different patterns
lucky :: (Integral a) => a -> String
lucky 7 = "LUCKY NUMBER SEVEN!"
lucky x = "Sorry, you're out of luck pal!"
-- calling lucky will check the patterns from top to bottom, and when it conforms, the corresponding function body will be used.
sayMe :: (Integral a) => a -> String
sayMe 1 = "One!"
sayMe 2 = "Two!"
sayMe 3 = "Three!"
sayMe 4 = "Four!"
sayMe 5 = "Five!"
sayMe x = "Not between 1 and 5"

-- factorial :: Integer -> Integer
-- factorial x = product [1..x]
-- if defined recursively, like in proper Math
factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n - 1)

-- -- pattern matching works for tuples too; without:
-- addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)
-- addVectors a b = (fst a + fst b, snd a + snd b)

-- with pattern matching:
addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

first :: (a, b, c) -> a
first (x, _, _) = x

second :: (a, b, c) -> b
second (_, y, _) = y

third :: (a, b, c) -> c
third (_, _, z) = z

-- -- you can also pattern match in list comprehensions
-- xs = [(1,3), (4,3), (2,4), (5,3), (5,6), (3,1)]
-- [a+b | (a,b) <- xs]
-- -- Lists themselves can also be used in pattern matching. You can match with the empty list [] or any pattern that involves : and the empty list
head' :: [a] -> a
head' [] = error "Can't call head on an empty list, dummy"
head' (x:_) = x

length' :: [a] -> Int
length' x = sum [ 1 | _ <- x ]

tell :: (Show a) => [a] -> String  
tell [] = "The list is empty"  
tell (x:[]) = "The list has one element: " ++ show x  
tell (x:y:[]) = "The list has two elements: " ++ show x ++ " and " ++ show y  
tell (x:y:_) = "This list is long. The first two elements are: " ++ show x ++ " and " ++ show y

length'' :: (Num b) => [a] -> b
length'' [] = 0
length'' (_:xs) = 1 + length'' xs

sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

-- patterns; with patterns you can break something up according to a pattern and bind it to names whilst still keeping a reference to the whole thing; e.g. xs@(x:y:ys) is a pattern that will match exactly the same thing as x:y:ys but we can easily get the whole list via xs instead of repeating the pattern again.

capital :: String -> String
capital "" = "Empty string, whoops!"
capital all@(x:xs) = "The first letter of " ++ all ++ " is '" ++ [x] ++ "'"
-- We use a pattern to avoid repeating ourselves when matching against a bigger pattern
-- NB You can't use ++ in pattern match
--
--
-- -- Guards
-- -- guards are a way of testing whether some property of a value are true or false. They also play really nicely with patterns.
densityTell :: (RealFloat a) => a -> String
densityTell density
    | density < 1.2 = "Wow! You're going for a ride in the sky"
    | density <= 1000.0 = "Have fun swimming, but watch out for sharks!"
    | 3==3 = "If it's sink or swim, you're going to sink"

-- a guard is a boolean expression. If it evaluates to True, the corresponding function body is used, otherwise it drops through to the next guard.
--
-- The last guard is oftern otherwise, which is an alias for True; it's a catch all expression. If all the guards of a function evaluate to False, evaluation falls through to the next pattern.
-- We can use guards with functions that take as many parameters as we want
densityTell' :: (RealFloat a)  => a -> a -> String
densityTell' mass volume
    | mass / volume < 1.2 = "Wow! You're going for a ride in the sky"
    | mass / volume <= 1000 = "Have fun swimming, but watch out for sharks!"
    | otherwise = "If it's sink or swim, you're going to sink"

max' :: (Ord a)  => a -> a -> a
max' a b
    | a > b = a
    | otherwise = b

myCompare :: (Ord a)  => a -> a -> Ordering
a `myCompare` b
    | a > b = GT
    | a == b = EQ
    | a < b = LT

-- Where syntactic sugar
densityTell'' :: (RealFloat a) => a -> a -> String
densityTell'' mass volume
    | density < 1.2 = "Wow! You're going for a ride in the sky"
    | density <= 1000.0 = "Have fun swimming, but watch out for sharks!"
    | otherwise = "If it's sink or swim, you're going to sink"
    where density = mass / volume

-- we put where after the guards and then define several names or function, which are visible across the guards; but are only visible to that function. Alignment of names defined in where is important
densityTell''' :: (RealFloat a) => a -> a -> String
densityTell''' mass volume
    | density < air = "Wow! You're going for a ride in the sky"
    | density <= water = "Have fun swimming, but watch out for sharks!"
    | otherwise = "If it's sink or swim, you're going to sink"
    where density = mass / volume
          air = 1.2
          water = 1000.0

-- where bindings aren't shared across function bodies of different patterns
-- You can also use where bindings to pattern match:
-- Ex
-- ...
-- where density = mass /volume
--       (air, water) = (1.2, 1000.0)

initials :: String -> String -> String
initials firstName lastName = [f] ++ ". " ++ [l] ++ "."
    where (f:_) = firstName
          (l:_) = lastName

-- You can also define functions in the where block
calcDensities :: (RealFloat a) => [(a, a)] -> [a]
calcDensities xs = [density m v | (m, v) <- xs]
    where density mass volume = mass / volume

-- Where bindings can also be nested. Common practice is to make a function and define some helper function in its where clause and then give those functions helper functions as well, each with its own where clause

-- let bindings
-- Synctatic construct that lets you bind to variables at the end of a function and the whole function can see them, including all guards. On the other hand, let bindings let you bind to variables anywhere and are expressions themselves, but are very local, so they don't span across guards. Let bindings can be used for pattern matching.
cylinder :: (RealFloat a) => a -> a -> a
cylinder r h = 
    let sideArea = 2 * pi * r * h
        topArea = pi * r^2
    in sideArea + 2 * topArea

-- The form is let <bindings> in <expression>. Names defined in the let part are accessible to the expression after the in part
-- The difference between let and where is that let bindings are expressions themeselves,while where are syntactic constructs. That means that, similarly to if statements, we can cram the let in statement almost everywhere.
--
-- [if 5 > 3 then "Woo" else "Boo", if 'a' > 'b' then "Foo" else "Bar"] 
-- 4 * (let a = 9 in a + 1) + 2
-- They can also be used to introduce functions in a local scope
-- [let square x = x * x in (square 5, square 3, square 2)]
-- We can separate several bindings in one line with semicolon
-- (let a = 100; b = 200; c = 300 in a*b*c, let foo="Hey "; bar = "there!" in foo ++ bar)
-- You can pattern match with let bindings 
-- (let (a,b,c) = (1,2,3) in a+b+c) * 100
-- We can also put let bindings inside list comprehensions
-- calcDensities :: (RealFloat a) => [(a, a)] -> [a]  
-- calcDensities xs = [density | (m, v) <- xs, let density = m / v]
-- We include a let inside a list comprehension much like we would a predicate, only it doesn't filter the list, it only binds to names. The names defined in a let inside a list comprehension are visible to the output function (the part before the |) and all predicates and sections that come after of the binding.
-- We omitted the in part of the let binding when we used them in list comprehensions because the visibility of the names is already predefined there. However, we could use a let in binding in a predicate and the names defined would only be visible to that predicate. The in part can also be omitted when defining functions and constants directly in GHCi. If we do that, then the names will be visible throughout the entire interactive session.
--
-- Case expressions
-- They take a variable and then execute block of code for specific values of that variables
--
-- In haskell, these are expressions. We can also do pattern matching with them. Case expressions are actually just synctactic sugar for function definitions. These two definitions do the same thing:
head'' :: [a] -> a  
head'' [] = error "No head for empty lists!"  
head'' (x:_) = x  

head''' :: [a] -> a
head''' xs = case xs of [] -> error "No head for empty lists!"
                        (x:_) -> x

-- case expression of pattern -> result  
--                    pattern -> result  
--                    pattern -> result  
--                    ...  
-- case expressions can be used pretty much anywhere.
describeList :: [a] -> String
describeList xs = "The list is " ++ case xs of [] -> "empty."
                                               [x] -> "a singleton list."
                                               xs -> "a longer list."
