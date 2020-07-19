(defpackage :uncommon-lisp-asd
  (:use :cl :asdf))

(in-package :uncommon-lisp-asd)

(defsystem uncommon-lisp
  :version "0.0.0"
  :author  "Jacob Reckhard"
  :license "BSD-3-Clause"
  :components ((:file "uncommon-lisp")
               (:file "fn")
               (:file "functions"))
  :description "A set of macros for a more pleasent common lisp")
