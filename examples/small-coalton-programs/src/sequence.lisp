(in-package #:small-coalton-programs)

;;;; This is a basic implementation of Purely Functional Random Access Lists
;;;; from Chris Okasaki's paper. It is a pure data structure with
;;;; O(log n) updates and indexing, and O(1) head, tail, and cons

(coalton-toplevel
  (define-type (Tree :a)
    (Leaf :a)
    (Node :a (Tree :a) (Tree :a)))

  (declare tree-lookup (Int -> Int -> (Tree :a) -> (Optional :a)))
  (define (tree-lookup size i t)
    (match i
      (0 (match t
	   ((Leaf x) (Some x))
	   ((Node x _ _) (Some x))))
      (_ (match t
	   ((Leaf _) None)
	   ((Node _ l r)
	    (let ((size-new (div size 2)))
	      (if (<= i size-new)
		  (tree-lookup size-new (- i 1) l)
		  (tree-lookup size-new (- (- i 1) size-new) r))))))))
  
  (declare tree-update (Int -> Int -> :a -> (Tree :a) -> (Tree :a)))
  (define (tree-update size i n t)
    (match i
      (0 (match t
	   ((Leaf _) (Leaf n))
	   ((Node _ l r) (Node n l r))))
      (_ (match t
	   ((Leaf _) t)
	   ((Node x l r)
	    (let ((size-new (div size 2)))
	      (if (<= i size-new)
		  (node x (tree-update size-new (- i 1) n l) r)
		  (node x l (tree-update size-new (- (- i 1) size-new) n r)))))))))
  
  (define-type (Seq :a)
    (SeqCons Int (Tree :a) (Seq :a))
    SeqNil)
  
  (declare seq-lookup (Int -> (Seq :a) -> (Optional :a)))
  (define (seq-lookup i t)
    (match t
      ((SeqCons s x xs)
       (if (< i s)
	   (tree-lookup s i x)
	   (seq-lookup (- i s) xs)))
      (_ None)))

  (declare seq-update (Int -> :a -> (Seq :a) -> (Seq :a)))
  (define (seq-update i n t)
    (match t
      ((SeqCons s x xs)
       (if (< i s)
	   (SeqCons s (tree-update s i n x) xs)
	   (SeqCons s x (seq-update (- i s) n xs))))
      (_ t)))
  
  (declare seq-cons (:a -> (Seq :a) -> (Seq :a)))
  (define (seq-cons n t)
    (match t
      ((SeqCons sa a (SeqCons sb b xs))
       (if (== sa sb)
	   (Seq-Cons (+ (+ sa sb) 1) (Node n a b) xs)
	   (Seq-Cons 1 (Leaf n) SeqNil)))
      (other
       (SeqCons 1 (Leaf n) other))))

  (declare seq-nil (Seq :a))
  (define seq-nil SeqNil)
  
  (define-instance (Functor Seq)
    (define (map f t)
      (match t
	((SeqCons s x xs)
	 (SeqCons s (match x
		      ((Node a l r)
		       (Node (f a) (map f l) (map f r)))
		      ((Leaf a)
		       (Leaf (f a))))
		  (map f xs)))
	(SeqNil SeqNil)))))
