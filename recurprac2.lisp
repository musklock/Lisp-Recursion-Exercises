
;;1.

(defun longest-list (list-of-lists)
	(cond
		((all-are-lists list-of-lists)
		 (values (all-are-lists list-of-lists) (long list-of-lists)))
		((not (all-are-lists list-of-lists)) (values nil nil))
	))


(defun all-are-lists(list-of-lists)
	(cond 
		((null list-of-lists) t)
		( (and (listp list-of-lists) (listp (car list-of-lists))) (all-are-lists (cdr list-of-lists)))))


(defun long (list-of-lists)
	(setq lst '())
	(long-rec list-of-lists lst))

(defun long-rec (list-of-lists lst)
	(cond
		((null list-of-lists) lst)				;;return the longest list
		((>(length (car list-of-lists)) (length lst))
		   (long-rec (cdr list-of-lists) (car list-of-lists)))
		((<(length (car list-of-lists)) (length lst)) 
		   (long-rec (cdr list-of-lists) lst))))

;;--------------------------------------------------------------------------------------------------

;;2.
;;TO-DO : reverse

(defun sequence1 (spacing low high)
	(cond
		((all-are-numbers spacing low high) 
		 (values (all-are-numbers spacing low high) (my-sequence spacing low high)))
		((not (all-are-numbers spacing low high)) (values nil nil))))

(defun all-are-numbers (spacing low high)
	(and (plusp spacing) (numberp low) (numberp high)))


(defun my-sequence (spacing low high)
	(setq lst '())
	(sequence-helper spacing low high lst))


(defun sequence-helper (spacing low high lst)
	(cond
		((> low high) (reverse lst))			;;if low>high, return list
		((or (< low high) (= low high)) 
		 (sequence-helper spacing (+ low spacing) high (cons low lst)))))


;;--------------------------------------------------------------------------------------------------

;;3.
(defun rec-multiply1 (x y)
	(cond
		((< x y) (rec-multiply y x))
		((not (equal y 0)) (+ x (rec-multiply x (- y 1))))
		((or (equal x 0) (equal y 0)) 0)))

(defun rec-multiply (x y)
	(let ((x-sign 
			(cond
				((or (> x 0) (= x 0)) 1)
				((< x 0) -1)))

		  (y-sign 
		  	(cond
				((or (> y 0) (= y 0)) 1)
				((< y 0) -1) )))

		(* (rec-multiply1 (abs x) (abs y)) x-sign y-sign)))


;;--------------------------------------------------------------------------------------------------

;;4.
;;returns true if all elements in 'ys' are contained in 'xs' and nil otherwise
;;Do I have to?: Use the function 'equal' to test for equality.

(defun contains-all (xs ys)
	(cond
		((null ys) t)
		((member (car ys) xs :test #'equal) (contains-all xs (cdr ys)))))

;;--------------------------------------------------------------------------------------------------

;;5.

(defun elfish? (str)
	(setq elf '(#\e #\l #\f))
	(contains-all (coerce str 'list) elf))

;;--------------------------------------------------------------------------------------------------

;;6.
;;adding suffixes

(defun my-map (func lst)
	(cond 
		((null lst) nil)
		((list lst) (cons (funcall func (car lst)) 
			   (my-map func (cdr lst))))))

 (defun string-append-map (xs str)
 	(my-map (lambda (x) (concatenate 'string x str)) xs))
;;(string-append-map '("run" "hide") ".v") -> ("run.v" "hide.v")

;;--------------------------------------------------------------------------------------------------

;;7.

(defun caps-no-x-list (str)
	(setq lst (coerce (string-upcase str) 'list))	
	(coerce (delete #\X lst) 'string)
)

;;--------------------------------------------------------------------------------------------------

;;8. 
;;TO-DO
;; Write a function 'filter-map-xs' which takes three arguments 'xs',
;;'filterfn', and 'mapfn' and returns a list where all elements of 'xs' where
;;'filterfn' returns nil is removed and the remaining elements are modified
;;according to 'mapfn'.

(defun filter-map-xs (xs filterfn mapfn)
	(mapcar mapfn (remove-if-not filterfn xs)))

;;(defun filter-map-xs (xs filterfn mapfn)
;;	(setq lst '())
;;	(map-helper xs filterfn mapfn lst)
;;)

(defun map-helper (xs filterfn mapfn lst)
	(mapcar mapfn (filter-helper xs filterfn lst)))

(defun filter-helper (xs filterfn lst)
	(cond
		((null xs) (reverse lst))
		((funcall filterfn (car xs))
		 (filter-helper (cdr xs) filterfn (cons (car xs) lst)))
		
		((not (funcall filterfn (car xs)))
		 (filter-helper (cdr xs) filterfn lst))))

(defun plusone (x) (+ x 1))
(defun isodd (x) (equal 1 (mod x 2)))

;;(filter-map-xs '(1 2 3 4) #'isodd #'plusone) 
;;--------------------------------------------------------------------------------------------------

;;9.
;;Write 'caps-no-x-list2' which has the same functionality as 'caps-no-x-list'
;;but is implemented using 'filter-map-xs' and two local or lambda functions.

;;(defun caps-no-x-list (str)
;;	(setq lst (coerce (string-upcase str) 'list))	
;;	(coerce (delete #\X lst) 'string)
;;)

;;(defun filter-map-xs (xs filterfn mapfn)
;;	(mapcar mapfn (remove-if-not filterfn xs)))


;;(defun caps-no-x-list2 (str)
;;	(setq lst (coerce (string-upcase str) 'list))
;;	(coerce (filter-map-xs lst 
;;		    (lambda (x) (not (equal #\X x))) (lambda (x) x)) 'string))



(defun caps-no-x-list2 (str)
	(let* ((lst (coerce (string-upcase str) 'list))
			(final-list (filter-map-xs lst (lambda (x) (not (equal #\X x))) (lambda (x) x) )) )
		(coerce final-list 'string)))

