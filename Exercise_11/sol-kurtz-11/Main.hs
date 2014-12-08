module Main where

import Data.List
import Directory.Demo

{----------------------------------- G1 ---------------------------------------}

import AssocList

{----------------------------------- G2 ---------------------------------------}

import Queue

{----------------------------------- G3 ---------------------------------------}

vocabs = ["i", "more", "more", "now", "want", "won", "wow"]

quasiIdentical :: Eq a => [a] -> [a] -> Bool
quasiIdentical [] [] = False
quasiIdentical (x : xs) (y : ys)
	| x == y    = quasiIdentical xs ys
	| otherwise = xs == ys
quasiIdentical _ _  = False

safeFixTypos :: [String] -> [String] -> [Either (String, [String]) String]
safeFixTypos vocabs = map go where
	go x
		| x `elem` vocabs = Right x
		| otherwise       = Left (x, nub $ filter (quasiIdentical x) vocabs)
