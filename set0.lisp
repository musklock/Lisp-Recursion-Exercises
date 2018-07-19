

(defun count-occur (s lst)
	(count-occur-helper s lst 0)
)

(defun count-occur-helper (s lst num)
	(cond
		((null lst) num)
		((atom (car lst))
			(count-occur-helper s (cdr lst) (+ num (check-if-equal s (car lst)))))
		((listp (car lst))

			(let ((real-num (count-occur-helper s (car lst) num))) 
				  (count-occur-helper s (cdr lst) real-num)))))


(defun check-if-equal (x y)
	(cond 
		((equal x y) 1)
		((not (equal x y)) 0)))

;;--------------------------------------------------------------------------------
;;2.
;;expr1 is the sub-expression of expr2 
;;TO-DO
(defun subexpr (expr1 expr2)
	(cond 
		((null expr2) nil)
		((and (and (atom expr1) (atom expr2)) (equal expr1 expr2)) t)

		((equal expr1 (car expr2)) expr2)
		((not (equal expr1 expr2)) 
			(subexpr expr1 (cdr expr2)))
	)
)

;;--------------------------------------------------------------------------------
;;3.
;;return all atoms in the original order
(defun my-flatten (lst)
	(setq my-lst '())
	(my-flatten-helper lst my-lst))

(defun my-flatten-helper (lst my-lst)
	(cond 
		((null lst) (reverse my-lst))
		((atom (car lst)) 
		 (my-flatten-helper (cdr lst) (cons (car lst) my-lst)))

		((listp (car lst)) 
		 (let ((list1 (my-flatten-helper (car lst) my-lst)))
		      (cons list1 (my-flatten-helper (cdr lst) my-lst)))
		 )))

;;--------------------------------------------------------------------------------
;;4. 
(defun my-intersection (l1 l2)

)


;;--------------------------------------------------------------------------------
;;5.
(defun fib (n)
	(let (
		  (a 0)
		  (b 1)
		  (c n))
	(loop for i from 2 to n do
		(print c)
		(setq c (+ a b))
		(setq a b)
		(setq b c))
		
		c)) 

(defun fib-list-implementation (n)
		(let (
			(fib-list '(1 0)))

		(loop for i from 2 to n do
			(cons (+ (car fib-list) (cadr fib-list)) fib-list))

			fib-list))


;;--------------------------------------------------------------------------------
;;6.
(defun merge-occurence-counts (lst1 lst2)

)

(defun make-second-list (lst2 count)	
	(setq new-lst2 (remove-duplicates lst2))
	(cond
		((null lst2) new-lst2)
		((equals (car new-lst2) (car )))
	))
;;TO-DO: finish function

(defun countx (lst2)
  (setq duplicate-removed-lst (remove-duplicates lst2))
  (setq counted-lst '())
  (count-occurences-in-second-list lst2 duplicate-removed-lst counted-lst)
  )

(defun count-occurences-in-second-list (lst2 duplicate-removed-lst counted-lst2)
  
  (cond
   ((null duplicate-removed-lst) counted-lst2)
   ((listp lst2) 
    (cons (list (count (car duplicate-removed-lst) lst2) (car duplicate-removed-lst)) 
          (count-occurences-in-second-list lst2 (cdr duplicate-removed-lst) counted-lst2)))
   
   )
  )

