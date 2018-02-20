(************************************************************************************************** 
			PROGRAMMING LANGUAGES - ASSIGNMENT 3: ML
			DANIEL RIVERA RUIZ - drr342@nyu.edu
***************************************************************************************************)
Control.Print.printDepth := 100;
Control.Print.printLength := 100;

(************************************************************************************************** 
1. Write a function foo whose type is: (’a -> ’b) -> (’b list -> ’c) -> ’a -> ’c list
***************************************************************************************************)
fun foo f g x = [g [f x]]

(************************************************************************************************** 
2. Write a function bar x, where x is an integer, that returns a function that takes a list L of
integers and returns a list each of whose elements result from multiplying the corresponding
element of L by x. Do not use the built-in map function. Rather, write any recursion that is
needed yourself. You should not define any function outside of bar.
***************************************************************************************************)
fun bar x = fn L => let 
	fun m f [] = [] | m f (c::cs) = (f c) :: m f cs
	in m (fn y => x * y) L
	end

(************************************************************************************************** 
3. In preparation for writing a partition sort function (below), define a function part that takes
an integer x and an integer list L as parameters and partitions L based on x. That is, part
returns a tuple of two lists, such that the first list contains all elements of L less than x and
the second list contains the elements of L that are greater than or equal to x.
***************************************************************************************************)
fun part x [] = ([], [])
  | part x (l::ls) =
	let val (left, right) = (part x ls)
	in if (l < x)
	then (l::left, right) 
	else (left, l::right)
	end

(************************************************************************************************** 
4. Implement a partition sorting function, partSort which uses your part function, above,
to sort a list L of integers, returning a sorted list of the elements of L. A partition sort is
essentially Quicksort, but without the property of using only a constant amount of space.
The algorithm, given a list L, is:
• If L is empty or contains a single element, return L (use pattern-matching for these two
cases, not a conditional).
• Otherwise, assuming L is of the form (x::xs), partition xs into two lists based on x by
calling your part function.
• Recursively call partSort on each of the two lists, to sort each of the lists.
• Return a single sorted list containing the elements from the first sorted list, followed by
x, followed by the elements of the second sorted list.
***************************************************************************************************)
fun partSort [] = []
  | partSort [a] = [a]
  | partSort (l::ls) =
	let val (left, right) = (part l ls)
	in (partSort left) @ [l] @ (partSort right)
	end

(************************************************************************************************** 
5. Implement a polymorphic partition sort function, pSort, that will sort a list of elements of
any type. In order to do this, though, pSort must take an additional parameter, the < (lessthan) 
operator, that operates on the element type of the list. 
***************************************************************************************************)
fun pSort (op <) [] = []
  | pSort (op <) [a] = [a]
  | pSort (op <) (m::ms) =
	let
	fun p (op <) x [] = ([], [])
  	  | p (op <) x (l::ls) = 
		let val (izquierda, derecha) = (p (op <) x ls)
		in if (l < x) 
		then (l::izquierda, derecha) 
		else (izquierda, l::derecha)
		end
	val (left, right) = (p (op <) m ms)
	in (pSort (op <) left) @ [m] @ (pSort (op <) right)
	end

(************************************************************************************************** 
6. Write a function reduce that takes a function f and a list L, such that if L is of the form
[x1, x2, ..., xn-1, xn] then reduce returns the result of f (x1 (f x2 ... (f xn-1 xn) ... )
***************************************************************************************************)
exception reduce_error
fun reduce f [] = raise reduce_error
  | reduce f [a] = a
  | reduce f (l::ls) = f l (reduce f ls)

(************************************************************************************************** 
7. Define a polymorphic tree datatype (i.e. datatype 'a tree = ...) such that a leaf is labeled
with an 'a and an interior node has a list of children, each of type 'a tree. That is, each 
interior node can have an arbitrary number of children, rather than just two (as in a binary tree).
***************************************************************************************************)
datatype 'a tree = leaf of 'a | node of 'a tree list

(************************************************************************************************** 
8. Define a function fringe t, where t is an 'a tree (above), which computes the fringe of t.
The fringe of a tree is a list of the values at the leaves of a tree. You should use your reduce 
function (along with map), allowing fringe to be written in roughly three lines.
***************************************************************************************************)
fun fringe (leaf l) = [l]
  | fringe (node []) = []
  | fringe (node (t)) = List.concat(map fringe t)

(************************************************************************************************** 
9. Write a polymorphic function sortTree (op <) t, where t is of type 'a list tree, that
returns a tree with the same structure as t, except that the list found at each leaf has been
sorted using your pSort function, above.
***************************************************************************************************)
fun sortTree (op <) (leaf l) = leaf (pSort (op <) l)
  | sortTree (op <) (node []) = node []
  | sortTree (op <) (node t) = node (map (sortTree (op <)) t)

(************************************************************************************************** 
10. Suppose that a set is represented in ML as a list. Define the function powerSet L, where L
is of type 'a list, that returns the power set of L. That is, it returns the list of all possible
sub-lists of L.
***************************************************************************************************)
fun powerSet [] = [[]]
  | powerSet (l::ls) =
	let val p = powerSet ls
     	fun ins (a, []) = []
     	  | ins (a, x::xs) = (a::x) :: ins (a, xs)
	in ins(l, p) @ p 
	end