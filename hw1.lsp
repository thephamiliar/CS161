;; 1. Write a single Boolean LISP function, called TREE-CONTAINS, which takes two 
;; arguments N and TREE, and checks whether number N appears in the ordered tree TREE.
(defun TREE-CONTAINS (N TREE)
;	"Checks whether number N appears in ordered tree TREE."
	(cond ((NULL TREE) NIL)
		((numberp TREE)
			(cond
				((= N TREE) t)
				(t NIL)
			)
		)
		(t (or (TREE-CONTAINS N (car TREE)) (TREE-CONTAINS N (cdr TREE))))
	)
)

;;2. Write a single LISP function, called TREE-MAX, which takes one argument TREE, and 
;;returns the maximum number appearing in the ordered tree TREE.

(defun TREE-MAX(TREE)
;	"Returns the max number in TREE."
		; if L and R are null
			; return m
		; else
			; return TREE-MAX with right subtree
		(cond ((NULL TREE) NIL)
			((numberp TREE) TREE)
			((equal NIL (cdr (cdr TREE))) (car TREE))
			(t (TREE-MAX (car (cdr (cdr TREE)))))
		)
)

;;3. Write a single LISP function, called TREE-ORDER, which takes one argument TREE, and 
;;returns an in-ordered list of the numbers appearing in ordered tree TREE.
(defun TREE-ORDER(TREE)
;	"Returns in-ordered list in ordered TREE."
		; if L and R are null
			; append m to list
		; else
			; TREE-ORDER with left subtree
			; append m to list
			; TREE-ORDER with right subtree
		; return list
)

;;4. Write a single LISP function, called SUB-LIST, that takes a list L and two 
;;non-negative integers START and LEN, and returns the sub-list of L starting at position 
;;START and having length LEN. Assume that the first element of L has position 0. 
(defun SUB-LIST(L START LEN)
;	"Returns sub-list of L from START and length LEN."
		; while start != 0
			; get rest of list
			; start--
		; while len != 0
			; get first element and add to sublist
			; len--
			; get rest of list
		; return sublist
)

;;5. Write a single LISP function, called SPLIT-LIST, that takes a list L, and returns a 
;;list of two lists L1 and L2, in that order, such that L is the result of appending L1 
;;and L2 and Length of L2 minus length of L1 is 0 or 1.
(defun SPLIT-LIST(L L1 L2)
;	"Returns two lists that splits list L."
		; count number of elements in list
		; temp = L
		; while there are still elements left
			; count++
			; temp = rest of temp
		; len = count / 2
		; temp = L
		; while len != 0
			; L1 = head of temp
			; L2 = rest of temp
			; temp = L2
			; len--
		; return L1 and L2
)

;;6. Write a single LISP function, called LIST2BTREE, that takes a non-empty list of 
;;atoms LEAVES, and returns a binary tree.
(defun LIST2BTREE(LEAVES)
;	"Returns binary tree."

)

;;7. Write a single LISP function, called BTREE2LIST, that takes a binary tree TREE, and 
;;returns a list of atoms (assume TREE follows the constraints we defined earlier).
(defun BTREE2LIST(TREE)
;	"Returns list from a binary tree."

)