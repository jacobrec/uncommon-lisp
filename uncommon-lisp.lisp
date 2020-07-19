(defpackage :uncommon-lisp
  (:nicknames :ucl)
  (:use cl))
  ;(:shadow print))

(in-package :uncommon-lisp)
(defun export-package (package)
  (let ((pack (find-package package)))
    (do-all-symbols (sym pack) (when (eql (symbol-package sym) pack) (export sym)))))

(export-package :common-lisp)
(export-package :uncommon-lisp)
