;; DANIEL RIVERA RUIZ
;; N10385937
;; drr342@nyu.edu

;; Programming Languages
;; Assignment 3

;; ......................................................................................................
;; 1.  (fromTo k n)
    ;; Returns the list of integers from k to n. 
    ;; The size of the problem can be seen as the number of integers between k and n, inclusive.
    ;; 1) Base Case: if k > n (i.e. if the size of the problem is 0), then the result is the empty list.
    ;; 2) Hypothesis: Assume (fromTo (+ k 1) n) returns the list of integers from k+1 to n, 
    ;;     since the size of that problem is one less than the size of the orignal problem, (fromTo k n).
    ;; 3) Recursive step: (fromTo k n) = (cons k (FromTo (+ k 1) n)

(define (fromTo k n)
  (cond ((> k n) '())
        (else (cons k (fromTo (+ k 1) n)))))

;; ......................................................................................................
;; 2.  (removeMults m L)
    ;; Returns a list containing all the elements of L that are not multiples of m. 
    ;; Size of the problem: length of the list L.
    ;; 1) Base Case: len(L) = 0, then the result is the empty list.
    ;; 2) Hypothesis: Assume (removeMults m cdr(L)) returns the list containing all the elements
    ;;    of cdr(L) that are not multiples of m. The size of this problem is one less than the size of
    ;;    the orignal problem, since len(cdr(L)) = len(L) - 1.
    ;; 3) Recursive step: append car(L) to the result of 2) if car(L)%m != 0.

(define (removeMults m L)
  (cond ((null? L) '())
        ((= (modulo (car L) m) 0) (removeMults m (cdr L)))
        (else (cons (car L) (removeMults m (cdr L))))))

;; ......................................................................................................
;; 3.  (removeAllMults L)
    ;; Given a list L containing integers in strictly increasing order, returns a list containing those 
    ;;    elements of L that are not multiples of each other.
    ;; Size of the problem: length of the list L.
    ;; 1) Base Case: len(L) = 0, then the result is the empty list.
    ;; 2) Hypothesis: Assume (removeAllMults (cdr L)) returns the list containing all the elements
    ;;    of cdr(L) that are not multiples of each other. The size of this problem is one less than 
    ;;    the size of the orignal problem, since len(cdr(L)) = len(L) - 1.
    ;; 3) Recursive step: (removeAllMults L) = (cons (car L) (removeMults (car L) (removeAllMults (cdr L)))).

(define (removeAllMults L)
  (cond ((null? L) '())
        (else (cons (car L) (removeMults (car L) (removeAllMults (cdr L)))))))

;; ......................................................................................................
;; 4.  (primes n)
    ;; Computes the list of all primes less than or equal to n.

(define (primes n) (removeAllMults (fromTo 2 n)))

;; ......................................................................................................
;; 5.  (maxDepth L)
    ;; Given a list L, it returns the maximum nesting depth of any element within L,
    ;;     such that the topmost elements are at depth 0.
    ;; Size of the problem: max level of nesting.
    ;; 1) Base case: if L is the empty list, return 0; if L is a scalar, return -1 (it means that the
    ;;    algorithm has already gone one level deeper than it should to get the max depth).
    ;; 2) Hypothesis: (maxDepth (cdr L)) returns the max level of nesting in (cdr L).
    ;; 3) Recursive step: (maxDepth L) returns the maximum value between (maxDepth (cdr L)) and
    ;;    (+ 1 (maxDepth (car L))). When the car of L is a scalar, this + 1 will even up with the - 1
    ;;    of the base case.

(define (maxDepth L)
  (cond ((null? L) 0) 
        ((not (list? L)) -1)
        (else (let ((x (+ 1 (maxDepth (car L))))
                    (y (maxDepth (cdr L))))
                (max x y)))))

;; ......................................................................................................
;; 6.  (prefix exp)
    ;; This function transforms an infix arithmetic expression exp into prefix notation.
    ;; Size of the problem: amount of operators in exp.
    ;; 1) Base case: if exp is an atom, return exp. if exp is a list of length 1, return its only value.
    ;; 2) Hypothesis: given expr = (car cadr cddr), assume (prefix car) and (prefix cddr) return the prefix
    ;;    notation for the operands.
    ;; 3) Recursive step: return the list L = '(cadr (prefix car) (prefix cddr)), where the cadr, which is
    ;;    the operator since it is in the second position of exp, is moved to the first position.

(define (prefix exp)
  (cond ((not (list? exp)) exp)
        ((= (length exp) 1) (prefix (car exp))) ; applying prefix here helps eliminate excesive parenthesis.
        (else (list (cadr exp) (prefix (car exp)) (prefix (cddr exp))))))

;; ......................................................................................................
;; 7.  (composition fns)
    ;; Takes a list of functions fns and returns a function that is the composition of the functions in fns.
    ;; Size of the problem: length of the list of functions.
    ;; 1) Base case: if the list contains only one function, return it.
    ;; 2) Hypothesis: asume that (composition (cdr fns)) returns the composition of functions in the cdr of fns.
    ;; 3) Recursive step: return ((car fns) (composition (cdr fns))), which adds the car of fns as the final
    ;;    composited function.

(define (composition fns)
  (if (not (list? fns)) fns
      (let ((f (car fns)))
           (if (null? (cdr fns)) f
               (let ((g (composition (cdr fns))))
                    (lambda (x) (f (g x))))))))

;; ......................................................................................................
;; 8.  (bubble-to-nth L N)
    ;; L is a list of numbers and N is an integer. The result should be a list containing all the elements
    ;; of L, except that the largest element among the first N elements of L is now the Nth element of the
    ;; resulting list, and the elements after the Nth element are left in their original order.
    ;; Size of the problem: N.
    ;; 1) Base case: if N <= 1, return L. If N > len(L), set N to len(L).
    ;; 2) Hypothesis: assume that (bubble-to-nth L (- N 1)) returns a list as specified in the description
    ;;    of the problem, but up to the first (N-1) elements of L.
    ;; 3) Recursive step: compare elements (N-1) and N and swap them if they are in inversed order.

(define (bubble-to-nth L N)
  (let* ((n (min N (length L))) (stop (- (length L) n)))
    (cond ((<= n 1) L)
          (else (letrec ((compare (lambda (l)
                                    (cond ((= (length (cdr l)) stop) l)
                                          (else (cond ((= 1 (length l)) l)
                                                      ((<= (car l) (cadr l)) (cons (car l) (compare (cdr l))))
                                                      (else (cons (cadr l) (compare (cons (car l) (cddr l)))))))))))
                  (compare L))))))

;; ......................................................................................................
;; 9.  (b-s L N)
    ;; Given L a list of numbers and N an integer, return the list containing the elements of L in their original
    ;; order except that the first N elements are in sorted order. This function should call bubble-to-nth above.
    ;; Size of the problem: N.
    ;; 1) Base case: if N <= 1, return L. If N > len(L), set N to len(L). (same as in 8.)
    ;; 2) Hypothesis: assume that (b-s L (- N 1)) returns L with ordered elements up to N-1.
    ;; 3) Recursive step: use "bubble-to-nth" to swap the N-1th and Nth elements if they are inverted.

(define (b-s L N) 
  (let ((n (min N (length L))))
    (cond ((<= n 1) L)
           (else (b-s (bubble-to-nth L n) (- n 1))))))

;; ......................................................................................................
;; 10. (bubble-sort L)
    ;; Calls b-s above to return a list of the elements of L in sorted order.

(define (bubble-sort L) (b-s L (length L)))
