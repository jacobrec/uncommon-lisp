#!/usr/bin/sbcl --script
(load "~/.sbclrc")
(asdf:load-system :uncommon-lisp)
(in-package :uncommon-lisp)

;;;; Unit tests
(multiple-value-bind (args body) (define-to-lets '() '((define a 1)
                                                       (define b 2)
                                                       (+ a b)))
  (assert (equal '(b a) args))
  (assert (equal '((let ((a 1))
                     (let ((b 2))
                       (+ a b))))
                 body)))

(assert (equal
         '((a 1 2) (funcall b 1 2)
           (funcall c 1 2) (d 1 2))
         (auto-funcall-args '(b c)
                            '((a 1 2) (b 1 2)
                              (c 1 2) (d 1 2)))))

(assert (equal
         '((let ((x 1))
             (funcall a 1 2)
             (b 1 2)))
         (auto-funcall-args '(a)
                            '((let ((x 1))
                                (a 1 2)
                                (b 1 2))))))

(assert (equal
         '((let ((x 1))
             '(a 1 2)
             '(funcall b 1 2)))
         (auto-funcall-args '(a)
                            '((let ((x 1))
                                '(a 1 2)
                                (quote (funcall b 1 2)))))))

(assert (equal
         '((let ((x 1))
             '(a 1 2)
             '(funcall b 1 2)))
         (auto-funcall-args '(a)
                            '((let ((x 1))
                                `(a 1 2)
                                `(funcall b 1 2))))))

(assert (equal
         '(x (funcall a 1 2))
         (auto-funcall-args '(a)
                             `(x (a 1 2)))))

(assert (equal
         '((x 1 (funcall a 8)) (funcall a 1 (funcall a 8)))
         (auto-funcall-args '(a)
                             '((x 1 (a 8)) (a 1 (a 8))))))

(assert (equal
         '(function
           (lambda (a c)
            (let ((b 1))
              (let ((x (fn (x) (* 2 x))))
                (funcall x (funcall a b c))))))
         (macroexpand '(fn (a c)
                        (define b 1)
                        (define x (fn (x) (* 2 x)))
                        (x (a b c))))))

;;;; End to end tests
#|
(defun test1 (a c)
  (let ((b 1))
    (let ((x (lambda (x) (* 2 x))))
      (funcall x (funcall a b c))))) |#
(defn test1 (a c)
  (define b 1)
  (define x (fn (x) (* 2 x)))
  (x (a b c)))

(assert (= 4 (test1 #'+ 1)))

#|
(defun test2 (a c)
  (let ((b 1))
    (let ((x (lambda (x) (* 2 x))))
      (multiple-value-bind (d e) (floor 5 4)
        (funcall x (funcall a b c d e)))))) |#
(defn test2 (a c)
  (define b 1)
  (define x (fn (x) (* 2 x)))
  (mdefine (d e) (floor 5 4))
  (x (a b c d e)))

(assert (= 8 (test2 #'+ 1)))

#|
(defun test3 ()
  (multiple-value-bind (s c) (values #'sin #'cos)
    (+ (funcall s 0) (+ funcall c 0)))) |#
(defn test3 ()
  (mdefine (s c) (values #'sin #'cos))
  (+ (s 0) (c 0)))

(assert (= 1 (test3)))

(println "All tests passed")
(sb-ext:exit)
