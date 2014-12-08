-- The line "module TExercise_1 where" allows this file to be imported by other Haskell files; it is optional if you only use one Haskell file at a time.
module TExercise_1 where

-- defines a generic function which concatenates several lists
concatenate :: [[a]] -> [a]
-- list comprehension with two generators
concatenate n = [x | xs <- n, x <- xs]