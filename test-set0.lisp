(load "set0.lisp")


(defun test-print (fn-name in)

  (let (out)
  
    (format t "====TEST-PRINT====~%")
    (format t "Function:~%s~%" fn-name)
    (format t "Input:~%s~%" in)
    (setq out (funcall fn-name in))
    (format t "Output:~%s~%" out)))

(defparameter *arguments* 'x '(x (y ((x y) f x))))

(test-print #'count-occur :arguments)
