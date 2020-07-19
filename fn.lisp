;;;; Better function names
(defun println (&rest args)
  (format t "~{~a~^ ~}~%" args))


;;;; Better defun macro,
;; Supports scheme style defines
;; Eliminates funcall
(defun define-to-lets (args body)
  (if (null body)
      (values args body)
      (if (eq 'define (caar body))
          (let ((var (cadar body))
                (val (caddar body)))
            (multiple-value-bind (newargs newbody) (define-to-lets (cons var args) (cdr body))
              (values newargs `((let ((,var ,val))
                                  ,@newbody)))))
          (multiple-value-bind (newargs newbody)
              (define-to-lets args (cdr body))
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
        ((not (listp (car body)))
         (cons
          (car body)
          (auto-funcall-args args (cdr body) is-quoted)))


        (t (let* ((body (mapcar #'macro-expand-if-not-fn-like body))
                  (is-quoted (if (eq 'quote (caar body))
                                (not is-quoted)
                                is-quoted)))
             (cons
              (if (member (caar body) args)
                  (if is-quoted
                      (car body)
                      `(funcall ,(caar body) ,@(auto-funcall-args args (cdar body) is-quoted)))
                  (auto-funcall-args args (car body) is-quoted))
              (auto-funcall-args args (cdr body)))))))


(defmacro fn (args &rest body)
  (multiple-value-bind (nargs body) (define-to-lets args body)
    `(lambda ,args ,@(auto-funcall-args nargs body))))

(defmacro defn (name args &rest body)
  `(setf (symbol-function ',name)
    (fn ,args ,@body)))

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
(defn test1 (a c)
  (define b 1)
  (define x (fn (x) (* 2 x)))
  (x (a b c)))
(assert (= 4 (test1 #'+ 1)))

