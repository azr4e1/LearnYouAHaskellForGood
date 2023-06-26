-- Haskell functions can take functions as parameters and return functions as return values. A function that does that is called higher order function.
--
-- Every function in Haskell officially takes only one parameter. All the functions that accept several parameters are actually **curried functions**. Let's see an example:
-- max 3 4; what it does is it creates a function that takes a parameter and returns either 4 or that parameter, depending on which one is bigger; then, 5 is applied to that function. The following are equivalent:
-- max 4 5
-- (max 4) 5
--
-- Putting a space between two things is _function application_. Space is sort of an operator with highest precedence.
-- if we call a function with too few parameters, we get back a partially applied function, meaning a function that takes as many parameters as we left out. Using partial application (calling functions with too few parameters, if you will) is a neat way to create functions on the fly so we can pass them to another function or to seed them with some data.
