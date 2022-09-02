module basetypes where

import Data.List (nub,sort)


-- Types for Exercise 1
--
type Bag a = [(a,Integer)]


-- Types and functions for Exercise 2
--
type Node  = Integer
type Edge  = (Node,Node)
type Graph = [Edge]
type Path  = [Node]

norm :: Ord a => [a] -> [a]
norm = sort . nub


-- Types for Exercise 3
--
type Number = Integer

type Point = (Number,Number)
type Length = Number

data Shape = Pt Point
           | Circle Point Length
           | Rect Point Length Length
           deriving Show

type Figure = [Shape]

type BBox = (Point,Point)
