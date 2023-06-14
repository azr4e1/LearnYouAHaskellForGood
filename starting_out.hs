-- -- Arithmetic
-- 2 + 15
-- 49 * 100
-- 1892 - 1472
-- 5 / 2
-- (50 * 100) - 4999
-- 50 * 100 - 4999
-- 50 * (100 - 4999)
--
-- -- Boolean
-- True && False
-- True && True
-- False || True
-- not False
-- not (True && True)
--
-- -- Equality
-- 5 == 5
-- 1 == 0
-- 5 /= 5
-- 5 /= 4
-- "hello" == "hello"
--
-- 5 + "llama" -- Error; non comparable
-- 5 == True -- Error: non comparable
--
-- -- Functions
-- succ 8
-- min 9 10
-- max 100 101
-- -- Function application has the highest precedence
-- succ 9 + max 5 4 + 1
-- -- same as
-- (succ 9) + (max 5 4) + 1
-- -- Prefix functions can be used as infix if surrounded by backtick (only if there are two arguments)
-- div 92 10
-- 92 `div` 10
--
-- -- Create first function
doubleMe x = x + x
-- doubleMe 9
-- doubleMe 8.3
--
doubleUs x y = x*2 + y*2
-- doubleUs 4 9
-- doubleUs 2.3 34.2
doubleUs x y = doubleMe x + doubleMe y
doubleSmallNumber x = if x > 100
                        then x
                        else x*2

-- Haskell conditional statements **always** require an else; every statement in Haskell must return  a value, i.e. must be an expression
-- doubleSmallNumber 101

-- When a function doesn't take any parameter, we call it a 'definition' or 'name'.
-- Also, apostrophes are valid as parameter/function names.
conanO'Brien = "It's a-me, Conan O'Brien!"

-- LISTS
-- Lists are homogenous data structure, that is they store elements of the same type
lostNumbers = [4, 8, 15, 16, 23, 42]
-- Strings are just a list of characters
-- [1, 2, 3, 4] ++ [9, 10, 11, 12]
-- "hello" ++ " " ++ "world!"
-- ['w', 'o', 'h', 'o', 'o']
-- Put something in front of a list with the `cons` operator
-- 'A' : " SMALL CAT"
-- 5:[1, 2, 3, 4, 5]
-- [1, 2, 3] is just syntactic sugar for 1:2:3:[], where [] is an empty list.
-- To index a list, we use !!
-- "Steve Buscemi" !! 6
-- [9.4,33.2,96.2,11.2,23.25] !! 1
-- Lists can contain lists; Lists can be compared if the stuff they contain can be compared. They are compared in lexicographical order.
-- [3, 2, 1] > [2, 1, 0]
-- [3, 2, 1] > [2, 10, 100]
-- [3, 4, 2] > [3, 4]
-- [3, 4, 2] == [3, 4, 2]
-- List functions:
-- head [5, 4, 3, 2, 1] -- first element
-- tail [5, 4, 3, 2, 1] -- all except first
-- last [5, 4, 3, 2, 1] -- last elements
-- init [5, 4, 3, 2, 1] -- all except last
-- length [5, 4, 3, 2, 1] -- length of list
-- null [5, 4, 3, 2, 1] -- check if list is empty
-- reverse [5, 4, 3, 2, 1] -- reverse list
-- take 3 [5, 4, 3, 2, 1] -- takes as many elements from start
-- drop 3 [5, 4, 3, 2, 1] -- drops elements from beginning of list
-- maximum [5, 4, 3, 2, 1]
-- minimum [5, 4, 3, 2, 1]
-- sum [5, 4, 3, 2, 1]
-- product [5, 4, 3, 2, 1]
-- elem 4 [5, 4, 3, 2, 1] -- check if it's part of list
-- 4 `elem` [5, 4, 3, 2, 1]
--
-- -- Ranges
-- [1..20] -- make a list of naturals from 1 to 20
-- ['a'..'z']
-- ['K'..'Z']
-- [2, 4..20] -- specify a step by giving the second example
-- [3,6..20]
-- [20,19..1]
-- -- You can also create infinite lists. Since Haskell is lazy, it won't evaluate the numbers untils it **needs** to
-- -- first 24 multiples of 13
-- [13, 26..24*13]
-- take 24 [13,26..]
-- take 10 (cycle [1, 2, 3]) -- cycle takes a list and cycles it into an infinite list
-- take 12 (cycle "LOL ")
-- take 10 (repeat 5) -- repeat takes an element and produces an infinite list of that element
-- replicate 10 5 -- same thing but better
-- -- List Comprehensions: this allows you to create list of elements coming from an input set,
-- -- which respect a predicate that you specify, 
-- -- and which are used as input for a custom function
-- -- Essentialy through the predicate we filter out the input
-- [x*2 | x <- [1..10]]
-- -- now adding a predicate
-- [x*2 | x <- [1..10], x*2 >= 12]
-- [x | x <- [50..100], mod x 7 == 3]
-- boomBangs xs = [ if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]
-- -- odd says whether the argument is odd or not
-- boomBangs [7..13]
-- -- we can use multiple predicates
-- [ x | x <- [10..20], x /= 13, x /= 15, x /= 19]
-- We can also draw from several lists. When drawing from multiple lists, comprehensions produce all combinations of the given lists and then join them by the output function we supply. E.g., get the cartesian product of two lists:
test = [ x*y | x <- [2, 5, 10], y <- [8, 10, 11] ]
test2 = [ x*y | x <- [2, 5, 10], y <- [8, 10, 11], x*y > 50 ]
nouns = ["hobo", "frog", "pope"]
adjectives = ["lazy", "grouchy", "scheming"]
test3 = [adjective ++ " " ++ noun | adjective <- adjectives, noun <- nouns]

length' xs = sum [1 | _ <- xs]
-- _ is throwaway variable
--
-- Remember: strings are also lists of characters
removeNonUpperCase st = [ c | c <- st, c `elem` ['A'..'Z']]
removeNonUpperCase "Hahaha! Ahahahaha!"
removeNonUpperCase "IdontLIKEFROGS"

let xxs = [[1,3,5,2,3,1,2,4,5],[1,2,3,4,5,6,7,8,9],[1,2,4,2,1,6,3,1,3,2,3,6]]
[[x | x <- xs, even x] | xs <- xxs]

-- Tuples
-- tuples are similar to lists: store several values into a single value; however, a list of numbers has type `list of numbers`. 
-- The type of a tuple depends on how many components it has and the type of the components. Also, they don't have to be homogenous. E.g.:
[[1, 2], [2, 3, 4], [3, 4]] -- representing points of a triange on a 2d plane: ok for haskell but it doesn't make sense
[(1, 2), (2, 3), (3, 4)] -- make sense; since a 2-value tuple is its own type:
[(1, 2), (2, 3, 4), (3, 4)] -- error because lists must be homogenous

-- Valid:
("Christopher", "Walken", 55)

fst (8, 11)
snd (8,11)

-- produces lists of pairs
zip [1, 2, 3, 4] [2, 3, 4, 5]
zip [5, 4, 3, 4, 5, 6, 7, 4, 3, 56] ["Im", "a", "turtle"]
-- because haskell is lazy, you can zip infinite lists
zip [1..] ["apple", "oranze", "cherry", "mango"]

-- Problem: which right triangle that has integers for all sides and all sides equal to or smaller than 10 has a perimeter of 24?
triangles = [(x, y, z) | x <- [1..10], y <- [1..10], z <- [1..10]]
right_triangles = [(x, y, z) | x <- [1..10], y <- [1..10], z <- [1..10], x**2 + y**2 == z**2]
final_right_triangles = [(x, y, z) | x <- [1..10], y <- [1..10], z <- [1..10], x**2 + y**2 == z**2, x+y+z == 24]
