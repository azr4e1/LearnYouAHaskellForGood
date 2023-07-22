-- Recursion is actually a way of defining functions in which the function is applied inside its own definition. Definitions in mathematics are often given recursively.
-- Having an element or two in a recursion definition defined non-recursivelyis also called the edge condition and is important if you want your recursive function to terminate.
-- Recursion is important to Haskell because unlike imperative languages, you do computations in Haskell by declaring what something is instead of declaring how you get it. That's why there are no while loops or for loops in Haskell and instead we many times have to use recursion to declare what something is.
--
-- Maximum algorithm
-- in imperative fashion (python):
-- curr_max = 0
-- for i in my_list:
--     if i > curr_max:
--         curr_max = i
-- 
-- in Haskell:
maximum' :: (Ord a) => [a]  -> a
maximum' [] = error "maximum of empty list"
maximum' [x] = x
maximum' (x:xs)
    | x > maxTail = x
    | otherwise = maxTail
    where maxTail = maximum' xs

maximum'' :: (Ord a) => [a] -> a
maximum'' [] = error "maximum of empty list"
maximum'' [x] = x
maximum'' (x:xs) = max x (maximum'' xs)

replicate' :: (Num i, Ord i) => i -> a -> [a]
replicate' n x
    | n <= 0    = []
    | otherwise = x:replicate' (n-1) x

take' :: (Num i, Ord i) => i -> [a] -> [a]
take' n _
    | n <= 0        = []
take' _ []          = []
take' n (x:xs)      = x:take' (n-1) xs

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

repeat' :: a -> [a]
repeat' x = x:repeat' x

zip' :: [a] -> [b] -> [(a, b)]
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x,y):zip' xs ys

elem' :: (Eq a) => a -> [a] -> Bool
elem' a [] = False
elem' a (x:xs)
    | a == x    = True
    | otherwise = a `elem'` xs

quickSort :: (Ord a) => [a] -> [a]
quickSort [] = []
quickSort (x:xs) = 
    let smallerSorted = quickSort [a | a <- xs, a <= x]
        biggerSorted = quickSort [a | a <- xs, a > x]
    in smallerSorted ++ [x] ++ biggerSorted


mergeSort :: (Ord a) => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs = 
    let len = length xs `div` 2
        x1 = mergeSort (take len xs)
        x2 = mergeSort (drop len xs)
    in joinOrdered x1 x2
    where joinOrdered :: (Ord a) => [a] -> [a] -> [a]
          joinOrdered [] [] = []
          joinOrdered xs [] = xs
          joinOrdered [] ys = ys
          joinOrdered xAll@(x:xs) yAll@(y:ys)
              | x <= y    = x:joinOrdered xs yAll
              | otherwise = y:joinOrdered xAll ys


insertionSort :: (Ord a) => [a] -> [a]
insertionSort [] = []
insertionSort [x] = [x]
insertionSort xs =
    let almostSorted = insertionSort (init xs)
        x = last xs
        smallerSorted = [w | w <- almostSorted, w <= x]
        biggerSorted =  [w | w <- almostSorted, w > x]
    in smallerSorted ++ [x] ++ biggerSorted

-- quicksort [2, 3, 1]
-- mergeSort [2, 3, 1]
-- insertionSort [2, 3, 1]
-- quicksort [10,2,5,3,1,6,7,4,2,3,4,8,9]
-- mergeSort [10,2,5,3,1,6,7,4,2,3,4,8,9]
-- insertionSort [10,2,5,3,1,6,7,4,2,3,4,8,9]
-- quicksort [7112, 29877, 29074, 19116, 31526, 27824, 13590, 12759, 1203, 5988, 16305, 416, 27995, 14290, 4888, 20656, 22923, 2783, 23254, 9737, 2063, 2745, 31768, 9133, 10474, 4344, 7086, 21154, 12400, 8573]
-- mergeSort [7112, 29877, 29074, 19116, 31526, 27824, 13590, 12759, 1203, 5988, 16305, 416, 27995, 14290, 4888, 20656, 22923, 2783, 23254, 9737, 2063, 2745, 31768, 9133, 10474, 4344, 7086, 21154, 12400, 8573]
-- insertionSort [7112, 29877, 29074, 19116, 31526, 27824, 13590, 12759, 1203, 5988, 16305, 416, 27995, 14290, 4888, 20656, 22923, 2783, 23254, 9737, 2063, 2745, 31768, 9133, 10474, 4344, 7086, 21154, 12400, 8573]
