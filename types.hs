
import basetypes

mybag = [(5,1),(7,3),(2,1),(3,2),(8,1)]

-- Exercise 1: Programming with Lists

-- (a) Define the function ins that inserts an element into a multiset.
ins :: Eq a => a -> Bag a -> Bag a
ins a [] = [(a,1)]
ins a ((x,y):ns)
	| a == x = (x,y+1):ns
	| otherwise = (x,y):ins a ns

-- (b) Define the function del that removes a single element from a multiset.
del :: Eq a => a -> Bag a -> Bag a
del a [] = []
del a ((x,y):ns)
	| a == x && y > 1 = (x,y-1):ns
	| a == x && y == 1 = ns
	| otherwise = (x,y):del a ns

-- (c) Define a function bag that takes a list of values and produces a multiset representation.
bag :: Eq a => [a] -> Bag a
bag [] = []
bag (x:xs) = ins x (bag xs)

-- (d) Define a function subbag that determines whether or not its first argument bag is contained in the second.
subbag :: Eq a => Bag a -> Bag a -> Bool
subbag [] _ = True
subbag _ [] = False
subbag ((x,y):l1) l2 = checkOneTuple (x,y) l2 && subbag l1 l2

-- check if a single tuple from a bag is contained inside another bag.
checkOneTuple _ [] = False
checkOneTuple (x1,y1) ((x2,y2):l2)
	| x1 == x2 && y1 <= y2 = True
	| otherwise = checkOneTuple (x1,y1) l2
	
-- (e) Define a function isSet that tests whether a bag is actually a set, which is the case when each element occurs only once.
isSet :: Eq a => Bag a -> Bool
isSet [] = True
isSet ((x,y):ls)
	| y == 1 = isSet ls
	| otherwise = False

-- (f) Define a function size that computes the number of elements contained in a bag.
size :: Bag a -> Integer
size [] = 0
size ((x,y):ls) = y+size ls

-- Exercise 2: Graphs

-- (a) Define the function nodes that computes the list of nodes contained in a given graph.
nodes :: Graph -> [Node]
nodes g = norm (map fst g ++ map snd g)

-- (b) Define the function suc that computes the list of successors for a node in a given graph.
suc :: Node -> Graph -> [Node]
suc n [] = []
suc n ((x,y):ls)
	| n == x = y:suc n ls
	| otherwise = suc n ls

-- (c) Define the function detach that removes a node together with all of its incident edges from a graph.
detach :: Node -> Graph -> Graph
detach node [] = []
detach node ((x,y):ls)
	| node == x || node == y = detach node ls
	| otherwise = (x,y):detach node ls

 -- (d) Define the function cyc that creates a cycle of any given number.
cyc :: Integer -> Graph
cyc 0 = []
cyc 1 = [(1,1)]
cyc n = zip [1..n] [2..n] ++ [(n,1)]

-- Exercise 3: Programming with Data Types

-- (a) Define the function width that computes the width of a shape.
width :: Shape -> Length
width (Pt (x,y)) = 0
width (Circle (x,y) r) = 2*r
width (Rect (x,y) w l) = w

-- (b) Define the function bbox that computes the bounding box of a shape.
bbox :: Shape -> BBox
bbox (Pt (x,y)) = ((x,y),(x,y))
bbox (Circle (x,y) r) = ((x-r,y-r),(x+r,y+r))
bbox (Rect (x,y) w l) = ((x,y),(x+w, y+l))

-- (c) Define the function minX that computes the minimum x coordinate of a shape.
minX :: Shape -> Number
minX (Pt (x,y)) = x
minX (Circle (x,y) r) = x-r
minX (Rect (x,y) w l) = x

-- (d) Define a function move that moves the position of a shape by a vector given by a point as its second argument.
move :: Shape -> Point -> Shape
move (Pt (x,y)) (xm,ym) = Pt (x+xm,y+ym)
move (Circle (x,y) r) (xm,ym) = Circle (x+xm,y+ym) r
move (Rect (x,y) w l) (xm,ym) = Rect (x+xm,y+ym) w l