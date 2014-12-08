module Othello_Base where

data Color = Black | White
  deriving (Eq, Show)

data Move = Pass | Play (Int, Int)
  deriving (Eq, Show)
