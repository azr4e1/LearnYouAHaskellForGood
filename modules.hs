-- a Haskell module is a collection of related function, types and typeclasses. A haskell program is a collection of modules where the main module loads up the other modules and then uses the functions defined in them to do something.
-- The Haskell standard library is split into modules, each of them contains functions and types that are somehow related and serve some common purpose. There's a module for manipulating lists, a module for concurrent programming, a module for dealing with complex numbers, etc. All the functions, types and typeclasses that we've dealt with so far were part of the Prelude module, which is imported by default.
--
-- The syntax for importing modules in a Haskell script is import <module name>. This must be done before defining any functions.
import Data.List

-- numUniques :: (Eq a) => [a] -> Int
-- numUniques = length . nub

-- When you do import Data.List, all the functions that Data.List exports become available in the global namespace, meaning that you can call them from wherever in the script. nub is a function defined in Data.List that takes a list and weeds out duplicate elements. Composing length and nub by doing length . nub produces a function that's the equivalent of \xs -> length (nub xs).
-- To import in GHCi you can do :m + <module>
-- If you just need a couple of functions from a module, you can selectively import just those functions. If we wanted to import only the nub and sort functions from Data.List, we'd do this:
import Data.List (nub, sort)
-- You can also choose to import all of the functions of a module except a few select ones. That's often useful when several modules export functions with the same name and you want to get rid of the offending ones. Say we already have our own function that's called nub and we want to import all the functions from Data.List except the nub function:
import Data.List hiding (nub)
-- Another way of dealing with name clashes is to do qualified imports. The Data.Map module, which offers a data structure for looking up values by key, exports a bunch of functions with the same name as Prelude functions, like filter or null. So when we import Data.Map and then call filter, Haskell won't know which function to use. Here's how we solve this:
import qualified Data.Map
-- This makes it so that if we want to reference Data.Map's filter function, we have to do Data.Map.filter, whereas just filter still refers to the normal filter we all know and love. But typing out Data.Map in front of every function from that module is kind of tedious. That's why we can rename the qualified import to something shorter:
import qualified Data.Map as M
-- To search for functions or to find out where they're located, use Hoogle: https://hoogle.haskell.org/
-- Data.List Module
-- Provides useful functions for dealing with lists. The Prelude module already exports some for convenience. Here's some functions from this module
-- intersperse takes an element and a list and puts that element inbetween each elements in the list.
a = intersperse '.' "MONKEY"
b = intersperse 0 [1, 2, 3, 4, 5, 6]

-- intercalate takes a list and a list of lists, and insert that list in between all those lists and flattens the result
c = intercalate " " ["hey", "there", "folks"]
d = intercalate [0,0,0] [[1,2,3],[4,5,6],[7,8,9]]
-- transpose transposes a list of lists
e = transpose [[1,2,3],[4,5,6],[7,8,9]]
-- Say we have the polynomials 3x2 + 5x + 9, 10x3 + 9 and 8x3 + 5x2 + x - 1 and we want to add them together. We can use the lists [0,3,5,9], [10,0,0,9] and [8,5,1,-1] to represent them in Haskell. Now, to add them, all we have to do is this:
f = map sum $ transpose [[0,3,5,9],[10,0,0,9],[8,5,1,-1]]
-- foldl' and foldl1' are stricter versions of their respective lazy incarnations. When using lazy folds on really big lists, you might often get a stack overflow error. The culprit for that is that due to the lazy nature of the folds, the accumulator value isn't actually updated as the folding happens. What actually happens is that the accumulator kind of makes a promise that it will compute its value when asked to actually produce the result (also called a thunk). That happens for every intermediate accumulator and all those thunks overflow your stack. The strict folds aren't lazy buggers and actually compute the intermediate values as they go along instead of filling up your stack with thunks. So if you ever get stack overflow errors when doing lazy folds, try switching to their strict versions.
--
-- NB: a thunk is a subroutine used to inject a calculation into another subroutine. Thunks are primarily used to delay a calculation until its result is needed, or to insert operations at the beginning or end of the other subroutine. The early years of compiler research saw broad experimentation with different evaluation strategies. A key question was how to compile a subroutine call if the arguments can be arbitrary mathematical expressions rather than constants. One approach, known as "call by value", calculates all of the arguments before the call and then passes the resulting values to the subroutine. In the rival "call by name" approach, the subroutine receives the unevaluated argument expression and must evaluate it.

-- A simple implementation of "call by name" might substitute the code of an argument expression for each appearance of the corresponding parameter in the subroutine, but this can produce multiple versions of the subroutine and multiple copies of the expression code. As an improvement, the compiler can generate a helper subroutine, called a thunk, that calculates the value of the argument. The address and environment[a] of this helper subroutine are then passed to the original subroutine in place of the original argument, where it can be called as many times as needed.
-- concat flattens a list of lists
g = concat ["foo", "bar", "car"]
h = concat [[3,4,5],[2,3,4],[2,1,1]]

-- Doing concatMap is the same as first mapping a function to a list and then concatenating the list with concat.
i = concatMap (replicate 4) [1..3]
-- and takes a list of boolean values and returns True only if all the values in the list are True.
l = and $ map (>4) [5,6,7,8]
-- or is like and, only it returns True if any of the boolean values in a list is True.
j = or $ map (==4) [2..6]
-- any and all take a predicate and then check if any or all the elements in a list satisfy the predicate, respectively. Usually we use these two functions instead of mapping over a list and then doing and or or.
k = any (==4) [2,3,4,5,6,7,1,4]
m = all (>4) [6,9,10]
n = all (`elem` ['A'..'Z']) "HEYGUYSwhatsup"
o = any (`elem` ['A'..'Z']) "HEYGUYSwhatsup"

-- iterate takes a function and a starting value, and applies the function to the value, to tis result, etc, in an infinite list
p = take 10 $ iterate (*2) 1
q = take 3 $ iterate (++ "haha") "haha"
-- splitAt takes a number and a list, it then splits the list at that many elements, returning the resulting two lists in a tuple
r = splitAt 3 "heyman"
r' = splitAt 100 "heyman"
r'' = splitAt (-3) "heyman"
r''' = let (a, b) = splitAt 3 "foobar" in b ++ a
-- dropWhile is similar to takeWhile, only it drops all the elemnts while the predicate is True. Once it is False, it returns the ret of the list
s = dropWhile (/=' ') "This is a sentence"
s' = dropWhile (<3) [1,2,2,2,2,3,4,5,6,5,4,6,43,3]

stock = [(994.4,2008,9,1),(995.2,2008,9,2),(999.2,2008,9,3),(1001.4,2008,9,4),(998.3,2008,9,5)]
t = head (dropWhile (\(val,y,m,d) -> val < 1000) stock)
-- span is similar to takeWhile, only it returns a pair of lists: the first containing everything the resulting list from takeWhile would contain, the second all the elements dropped
u = let (fw, rest) = span (/=' ') "This is a sentence" in "First word:" ++ fw ++ ", the rest:" ++ rest
-- break is kinda the opposite: span spans the list while the predicate is true, while break breaks it when the predicate is first true. `break p` is equivalent to `span (not . p)`
u' = break (==4) [1,2,3,4,5,6,7,8]
u'' = span (==4) [1,2,3,4,5,6,7,8]
-- sort sorts a list. The type of elements in the list must belong to the Ord typeclass
v = sort [8,5,3,2,1,6,4,2]
-- group takes a list and groups adjacent elements into sublists if they are equal.
w = group [1,1,1,1,2,2,2,2,3,3,2,2,2,5,6,7]
-- sorting a list before grouping allows us to find out how many unique elements there are, and how many times each element appears in the list
w' = map (\l@(x:xs) -> (x,length l)) . group . sort $ [1,1,1,1,2,2,2,2,3,3,2,2,2,5,6,7]
-- inits "ciao"
-- tail "ciao"
-- tails "ciao"
-- let w = "w00t" in zip (inits w) (tails w)
-- we can use fold to implement searching a list for a sublist
search :: (Eq a) => [a] -> [a] -> Bool
search needle haystack = 
    let nlen = length needle
    in foldl (\acc x -> if take nlen x == needle then True else acc) False (tails haystack)

-- isInfixOf searches for a sublist within a list and returns True if the sublist we're looking for is somewhere inside the target list.
-- "cat" `isInfixOf` "im a cat burglar"
-- isPrefixOf and isSuffixOf search for a sublist at the beginning and at the end of a list, respectively.
-- "hey" `isPrefixOf` "hey there!"
-- "there!" `isSuffixOf` "oh hey there!"
-- elem and notElem check if an element is or isn't inside a list.
-- partition takes a list and a predicate and returns a pair of lists. The first list in the result contains all the elements that satisfy the predicate, the second contains all the ones that don't.
-- partition (`elem` ['A'..'Z']) "BOBsidneyMORGANeddy"
-- partition (>3) [1,3,5,6,3,2,1,0,3,7]
-- find takes a list and a predicate and returns the first element that satisfies the predicate. But it returns that element wrapped in a Maybe value. We'll be covering algebraic data types more in depth in the next chapter but for now, this is what you need to know: a Maybe value can either be Just something or Nothing. Much like a list can be either an empty list or a list with some elements, a Maybe value can be either no elements or a single element. And like the type of a list of, say, integers is [Int], the type of maybe having an integer is Maybe Int. Anyway, let's take our find function for a spin.
-- find (>4) [1,2,3,4,5,6]
-- find (>9) [1,2,3,4,5,6]
-- head is not safe, empty list throw error; find is safe:
t' = find (\(val,y,m,d) -> val>1000) stock

-- elemIndex is kind of like elem, only it doesn't return a boolean value. It maybe returns the index of the element we're looking for. If that element isn't in our list, it returns a Nothing.
