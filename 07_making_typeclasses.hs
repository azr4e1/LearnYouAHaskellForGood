-- -- So far, we've run into a lot of data types. Bool, Int, Char, Maybe, etc. But how do we make our own? Well, one way is to use the data keyword to define a type. Let's see how the Bool type is defined in the standard library.
-- data Bool = False | True
-- -- data means that we're defining a new data type. The part before the = denotes the type, which is Bool. The parts after the = are value constructors. They specify the different values that this type can have. The | is read as or. So we can read this as: the Bool type can have a value of True or False. Both the type name and the value constructors have to be capital cased.
-- -- data Shape = Circle Float Float Float | Rectangle Float Float Float Float
-- -- The Circle value constructor has three fields, which take floats. So when we write a value constructor, we can optionally add some types after it and those types define the values it will contain. Here, the first two fields are the coordinates of its center, the third one its radius. The Rectangle value constructor has four fields which accept floats. The first two are the coordinates to its upper left corner and the second two are coordinates to its lower right one.
-- -- Value constructors are actually functions that ultimately return a value of a data type.
-- a = Circle 1 2 3
--
-- surface :: Shape -> Float
-- surface (Circle _ _ r) = pi * r ^ 2
-- surface (Rectangle x1 y1 x2 y2) = abs (x2 - x1) * abs (y2 - y1)
-- -- The first notable thing here is the type declaration. It says that the function takes a shape and returns a float. We couldn't write a type declaration of Circle -> Float because Circle is not a type, Shape is. Just like we can't write a function with a type declaration of True -> Int. The next thing we notice here is that we can pattern match against constructors. We pattern matched against constructors before (all the time actually) when we pattern matched against values like [] or False or 5, only those values didn't have any fields. We just write a constructor and then bind its fields to names.
-- --
-- -- if we try to just print out Circle 10 20 5 in the prompt, we'll get an error. That's because Haskell doesn't know how to display our data type as a string (yet). Remember, when we try to print a value out in the prompt, Haskell first runs the show function to get the string representation of our value and then it prints that out to the terminal. To make our Shape type part of the Show typeclass, we modify it like this:
-- -- data Shape = Circle Float Float Float | Rectangle Float Float Float Float deriving (Show)
--
-- -- constructors are functions, so we can map them and partially apply them (curried functions):
-- b = map (Circle 0 0) [4, 5, 6, 6]
--
data Point = Point Float Float deriving (Show)
data Shape = Circle Point Float | Rectangle Point Point deriving (Show)
-- Notice that when defining a point, we used the same name for the data type and the value constructor. This has no special meaning, although it's common to use the same name as the type if there's only one value constructor. So now the Circle has two fields, one is of type Point and the other of type Float. This makes it easier to understand what's what. Same goes for the rectangle. We have to adjust our surface function to reflect these changes.

surface :: Shape -> Float
surface (Circle _ r) = pi * r ^ 2
surface (Rectangle (Point x1 y1) (Point x2 y2)) = abs (x2 - x1) * abs (y2 - y1)
-- The only thing we had to change were the patterns. We disregarded the whole point in the circle pattern. In the rectangle pattern, we just used a nested pattern matching to get the fields of the points. 
nudge :: Shape -> Float -> Float -> Shape
nudge (Circle (Point x1 y1) r) a b = Circle (Point (x1 + a) (y1 + b)) r
nudge (Rectangle (Point x1 y1) (Point x2 y2)) a b = Rectangle (Point (x1+a) (y1+b)) (Point (x2+a) (y2+b))
c = nudge (Circle (Point 34 34) 10) 5 10

baseCircle :: Float -> Shape
baseCircle r = Circle (Point 0 0) r

baseRect :: Float -> Float -> Shape
baseRect width height = Rectangle (Point 0 0) (Point width height)

d = nudge (baseCircle 4) 3 2



-- record syntax
-- data Person = Person String String Int Float String String deriving (Show)
-- guy = Person "Buddy" "Frankestein" 30 184.2 "073942923" "Vanilla"
--
-- -- if we wanted to get separate infos from this person, we would need to write something like this:
-- firstName :: Person -> String  
-- firstName (Person firstname _ _ _ _ _) = firstname  
--   
-- lastName :: Person -> String  
-- lastName (Person _ lastname _ _ _ _) = lastname  
--   
-- age :: Person -> Int  
-- age (Person _ _ age _ _ _) = age  
--   
-- height :: Person -> Float  
-- height (Person _ _ _ height _ _) = height  
--   
-- phoneNumber :: Person -> String  
-- phoneNumber (Person _ _ _ _ number _) = number  
--   
-- flavor :: Person -> String  
-- flavor (Person _ _ _ _ _ flavor) = flavor 

-- however using record syntax, we can make this way less cumbersome:
data Person = Person
  { firstName :: String,
    lastName :: String,
    age :: Int,
    height :: Float,
    phoneNumber :: String,
    flavor :: String
  }
  deriving (Show)
-- The main benefit of this is that it creates functions that lookup fields in the data type. By using record syntax to create this data type, Haskell automatically made these functions: firstName, lastName, age, height, phoneNumber and flavor.
--  When we derive Show for the type, it displays it differently if we use record syntax to define and instantiate the type:
-- data Car = Car String String Int deriving (Show)
-- ghci> Car "Ford" "Mustang" 1967
-- Car "Ford" "Mustang" 1967
-- With record syntax however:
data Car = Car {
    company :: String,
    model :: String,
    year :: Int
    } deriving (Show)
-- now we don't have to necessarily put the fields in the proper order, as long as we list all of them.
d' = Car {company="Ford",
          model="Mustang",
          year=1967}
d'' = Car "Ford" "Mustang" 1968

-- Type parameters
-- A value constructor can take some values parameters and then produce a new value. For instance, the Car constructor takes three values and produces a car value. In a similar manner, type constructors can take types as parameters to produce new types.
data Maybe a = Nothing | Just a
-- A value constructor can take some values parameters and then produce a new value. For instance, the Car constructor takes three values and produces a car value. In a similar manner, type constructors can take types as parameters to produce new types.
-- The list type is also a type constructor: the list type takes a parameter to produce a concrete type; you can't have a value that has just a type of []
-- Type parameters are useful because we can make different types with them depending on what kind of types we want contained in our data type.
-- The type of Nothing is Polymorphic: Maybe a. If some function requires a Maybe Int as a parater, we can give it a Nothing, since Maybe a can act like a Maybe Int if it has to. Similar case is the empty list.
-- Using type parameters is very beneficial, but only when using them makes sense. Usually we use them when our data type would work regardless of the type of the value it then holds inside it, like with our Maybe a type.
-- we could change our Car type to be a type constructor:
data Car' a b c = Car' {
    company' :: a,
    model' :: b,
    year' :: c
    } deriving (Show)
-- But would we really benefit? The answer is: probably no, because we'd just end up defining functions that only work on the Car String String Int type. For instance, given our first definition of Car, we could make a function that displays the car's properties in a nice little text.
tellCar :: Car -> String
tellCar (Car {company = c, model = m, year = y}) = "This " ++ c ++ " " ++ m ++ " was made in " ++ show y
stang = Car {company="Ford", model="Mustang", year=1967}
-- but if the Car was Car a b c:
tellCar' :: (Show a) => Car' String String a -> String
tellCar' (Car' {company' = c, model' = m, year' = y}) = "This " ++ c ++ " " ++ m ++ " was made in " ++ show y
-- We'd have to force this function to take a Car type of (Show a) => Car String String a. 
-- We usually use type parameters when the type that's contained inside the data type's various value constructors isn't really that important for the type to work.
-- We can also add typeclass constraints in the data declaration:
-- data (Ord k) => Map k v = ...
-- However, it's a very strong convention in Haskell to never add typeclass constraints in data declarations. Why? Well, because we don't benefit a lot, but we end up writing more class constraints, even when we don't need them.

data Vector a = Vector a a a deriving (Show)

vplus :: (Num t) => Vector t -> Vector t -> Vector t
vplus (Vector i j k) (Vector l m n) = Vector (i+l) (j+m) (k+n)

vectMult :: (Num t) => Vector t -> t -> Vector t
vectMult (Vector i j k) m = Vector (i*m) (j*m) (k*m)

scalarMult :: (Num t) => Vector t -> Vector t -> t
scalarMult (Vector i j k) (Vector l m n) = (i*l) + (j*m) + (k*n)

data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday 
    deriving (Eq, Ord, Show, Read, Bounded, Enum)
-- nullary value constructors
e = [minBound :: Day .. maxBound :: Day]
e' = succ Sunday
e'' = [minBound .. maxBound] :: [Day]

-- type aliases
type Phonebook = [(String, String)]
