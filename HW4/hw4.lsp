; Jessica Pham
; UID: 004153744

; reload function for easy recompile
(defun reload()
	(load "hw4.lsp")
)


; top level function for N queens problem
(defun QUEENS (N)
	(mult-dfs '() 1 N)
)

; check for final state
; if placed all queens, reached final state
(defun final-state (s N)
	(cond ((not (equal (length s) N)) NIL)
		(T T)
	)
)

; checks if any of queens are in the same column
; s is the list of column numbers the queens are placed
; c is the column number to be checked
; N is the size of the N queens problem
; returns true if there is a violation
; returns nil if there are no violations
(defun check-columns(s c N)
	(cond ((OR (NULL s) (> c N)) NIL)
		((equal (car s) c) T)
		(T (OR (check-columns (cdr s) c N) (check-columns (cdr s) (car s) N)))
	)
)

; checks if any of the queens are diagnol from any other queen
; s is the list of column numbers the queens are placed
; row_s is the number of rows away from recently placed queen
; c is the column the new queen has been placed in
(defun check-diagnols (s row_from_s c)
	; calculate what the column should be
	; check row if equal
	; else check next row
	(cond ((NULL s) NIL)
		((OR (equal (car s) (- c row_from_s)) (equal (car s) (+ c row_from_s))) T)
		(T (check-diagnols (cdr s) (- row_from_s 1) c))
	)
)

; checks if next state is legal
; checks if violates any constraints
; returns column placement if no contraints violated
; returns nil otherwise
(defun is-valid(s c N)
	(let* ((potential-state (append s (list c))))
		(cond ((NULL s) (list c))
			((OR (check-columns s c N) (check-diagnols s (length s) c)) NIL)
			(t (list c))
		)
	)
)

; returns all of the possible legal successor states to the current
; state. It takes a single argument (s), which encodes the current state, and
; returns a list of possible next states
(defun succ-fn(s count N)
	(cond ((<= count N) (append (is-valid s count N) (succ-fn s (+ count 1) N)))
		(t NIL)
	)
)

; expand the node
(defun mult-dfs(s r N)
	(cond ((final-state s N) s)
		  (t (dfs s r N (succ-fn s 1 N)))
	)
)

; depth first search
; if first valid move is valid, perform dfs
; else perform dfs on the rest of the valid moves
(defun dfs(s r N valid-moves)
	(cond ((NULL valid-moves) NIL)
		  (t 
		  		(let ((next-state (mult-dfs (append s (list (car valid-moves))) (+ 1 r) N)))
		  			(cond ((not (NULL next-state)) next-state)
		  				  (t (dfs s r N (cdr valid-moves)))))))
)