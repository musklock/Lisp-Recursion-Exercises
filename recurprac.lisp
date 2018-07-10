;;- Count list length without length predicate recursively
;;- Index a list recursively [ ‘(a b c) => ‘((0 a) (1 b) (2 c)) ]
;;- Implement map
;;- Implement reduce
;;- All elements in list are true (not nil)
;;- Drop nth element in a list
;;- Depth first search of tree -- (perhaps return the pre-ordering of elements to check it works)

;;Count list length without length predicate recursively
(defun test (list a)
	(cond
		((listp list) T)
		)
	)

(defun length-of-list (list)
	(cond
		((null list) 0)
		(t (+ 1 (length-of-list (cdr list)))
		)
	)
)





;; Depth first traversal of a tree

(defun left-child (list)
 	(cond
 		((null list) nil)
 		((not (listp list)) nil)
 		(t (cadr list))

 	)

 )

(defun right-child (list)
 	(cond
 		((null list) nil)
 		((not (listp list)) nil)
 		(t (caddr list)) 

 	)

)

;;borrowed
(defun print-list (lst)
  (if (null lst)
    (format t "~%")
    (progn
      (format t "~a~%" (car lst))
      (print-list (cdr lst)))))



(defun depth-first-search (list)
 	;;(format t "~a~%" (car list))
 	(format t "~s " (car list))
 	(cond

 		((null list) nil)
 		((atom list) (format t "~s " list))
 		((equal (car list) nil)nil)
 		;;((< (length list) 3) nil)
 		;;(t(car list))
 		(t (progn
 			 (depth-first-search(left-child list)))
 			 (depth-first-search(right-child list)))
 	 

 	)
 	(format nil "end of func")
 	;;(print-list list)
 )

 ;;all elements in a list are true

 (defun all-are-true (list)
  	(cond 
  		((null list) t)
  		((equal (car list) t) 
  		 (all-are-true (cdr list)))
  	)

 )
  
    ;;drop nth element from list 
    ;;(nthcdr n list)
 (defun drop (n list)
    	(cond
    		((zerop n) (cdr list))
    		((cons (car list) (drop (1- n) (cdr list))))
  	)
 )


  


;;TO-DO: Index a list recursively [ ‘(a b c) => ‘((0 a) (1 b) (2 c)) ]

(defun index (list)
	(index-helper list (length list))
)

(defun index-helper (list1 list-length)
	(cond
		((null list) list)
		((list (list (- (- list-length (length (cdr list))) 1) (car list) ) (cdr list))
		 (index helper (cdr list) list-length))
	)
)

;;TO-DO: Implement map

(defun map (func list)
	(cond 
		((null list) t)
		((cons (funcall #'func (car list)) (cdr list)))

	)

)

;;TO-DO: Implement reduce 
(defun reduce (func list)
	(cond
		((null list) t)
		((cons (funcall  #'func (car list cadr list)) (cddr list)))
	)

)


(defun my-map (func lst)
	(cond 
		((null lst) nil)
		((list lst) (cons (funcall func (car lst)) 
			   (my-map func (cdr lst))))))

(defun my-reduce (func lst)
	(cond
		((= (length lst) 1) (car lst))
		;;((cons (funcall  #'func (car lst cadr lst)) (cddr lst)))
		((list lst) 
		 (my-reduce func 
			(let ((newfirst (funcall func (car lst) (cadr lst)))
				  (ourrest (cddr lst)) )
				(cons newfirst ourrest) ) )
	))

)


(defun funcname (v1 v2 v3)
	(let ((procv1 (proccessing v1))
		  (combined (combine v1 v2 v3))
		  localvar1 localvar2 localvar3)
		(localvar1 (read-from-a-file))
		...
		retur))



(defun my-reduce2 (func lst)
	(cond
		((= (length lst) 1) (car lst))
		;;((cons (funcall  #'func (car lst cadr lst)) (cddr lst)))
		((list lst)
		 (let* ((newfirst (funcall func (car lst) (cadr lst)))
		 	    (ourrest (cddr lst))
		 	    (newlst (cons newfirst ourrest)))
		 	(my-reduce func newlst)))))

(defun index (lst)
	(labels
		((index-helper (lst index)
			(if (null lst) 
				;; if true
				nil
				;; if false
				(cons (list index (car lst)) 
			  		(index-helper (cdr lst) (+ index 1)))))
		  ) ; end of labels definitions
		(index-helper lst 0))
)


(defun index-helper (lst index)
	(if (null lst) 
		;; if true
		nil
		;; if false
		(cons (list index (car lst)) 
			  (index-helper (cdr lst) (+ index 1)))))