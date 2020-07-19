(in-package :uncommon-lisp)

;;;; Better function names
(defun println (&rest args)
  (format t "~{~a~^ ~}~%" args))
;(defun print (&rest args) (format t "~{~a~^ ~}" args))
(defun newline ()
  (terpri))
