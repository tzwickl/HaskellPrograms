module Directory.Demo (
	T1,                 -- Export T1 but not C1A or C1B
	T2 (),              -- same as above
	T3 (C3A),           -- Export T3 and C3A but not C3B
	T4 (C4A, C4B),      -- Export T4 and also C4A and C4B
	T5 (..),            -- same as above
	foo,                -- Export the "foo" function
	Directory.Demo.sort -- Export the locally-defined "sort" function
) where

import Data.List

data T1 = C1A | C1B
data T2 = C2A | C2B
data T3 = C3A | C3B
data T4 = C4A | C4B
data T5 = C5A | C5B

foo :: Int -> Int
foo x = x + 1

sort :: Int -> Int
sort x = x - 1
