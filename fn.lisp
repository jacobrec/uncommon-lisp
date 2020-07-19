(in-package :uncommon-lisp)


;;;; Better defun macro,
;; Supports scheme style defines
;; Eliminates funcall
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun define-to-lets (args body)
    (cond ((null body) (values args body))
          ((eq 'define (caar body))
           (let ((var (cadar body))
                 (val (caddar body)))
             (multiple-value-bind (newargs newbody) (define-to-lets (cons var args) (cdr body))
               (values newargs `((let ((,var ,val))
                                   ,@newbody))))))
          ((eq 'mdefine (caar body))
           (let ((vars (cadar body))
                 (val (caddar body)))
             (multiple-value-bind (newargs newbody) (define-to-lets (append vars args) (cdr body))
               (values newargs `((multiple-value-bind ,vars ,val
                                   ,@newbody))))))
          (t (multiple-value-bind (newargs newbody) (define-to-lets args (cdr body))
                (values newargs (cons (car body) newbody))))))

  (defun macro-expand-if-not-fn-like (body)
    (if (and (listp body)
             (not (or (eq (car body) 'defn)
                      (eq (car body) 'fn))))
      (macroexpand body)
      body))
  (defun auto-funcall-args (args body &optional (is-quoted nil))
    (cond ((null body) body)
          ((not (listp body)) body)
          ((and (listp (car body)) (listp body))
           (cons
            (auto-funcall-args args (car body) is-quoted)
            (auto-funcall-args args (cdr body) is-quoted)))

          ;; (lambda (x) (x 2)) => (lambda (x) (funcall x 2))
          ((or (eq 'fn (car body)) (eq 'lambda (car body)))
           (let ((head (car body))
                 (declares (cadr body))
                 (bodies (mapcar (lambda (x) (car (auto-funcall-args args (list x)))) (cddr body))))
             `(,head ,declares ,@bodies)))

          ;; (let ((x (a 1))) (+ x 2))
          ((or (eq 'let* (car body)) (eq 'let (car body)))
           (let ((head (car body))
                 (declares (mapcar
                            (lambda (x) (list (car x) (auto-funcall-args args (cadr x))))
                            (cadr body)))
                 (bodies (mapcar (lambda (x) (car (auto-funcall-args args (list x)))) (cddr body))))
             `(,head ,declares ,@bodies)))

          ;; any other non forms stage
          (t (let* ((body (macro-expand-if-not-fn-like body))
                    (is-quoted (if (eq 'quote (car body))
                                  (not is-quoted)
                                  is-quoted)))
               (if (member (car body) args)
                   (if is-quoted
                       body
                       `(funcall ,(car body) ,@(map 'list
                                                 (lambda (x) (auto-funcall-args args x is-quoted))
                                                 (cdr body))))
                   (map 'list (lambda (x) (auto-funcall-args args x is-quoted)) body)))))))


(defmacro fn (args &rest body)
  (multiple-value-bind (nargs body) (define-to-lets args body)
    `(lambda ,args ,@(auto-funcall-args nargs body))))

(defmacro defn (name args &rest body)
  `(setf (symbol-function ',name)
    (fn ,args ,@body)))
