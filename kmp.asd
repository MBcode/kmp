#|
  This file is a part of kmp project.
  Copyright (c) 2013 Mike Bobak (bobak@balisp.org)
|#
(defun al (l) "asdf load" (asdf:oos 'asdf:load-op l))
#+quicklisp (defun ql (a) (ql:quickload  a :verbose t :explain t)) 
#|
  Author: Mike Bobak (bobak@balisp.org)
|#

(in-package :cl-user)
(eval-when (:compile-toplevel :load-toplevel :execute)
    (unless (find-package :kmp-asd) ;added
    (defpackage kmp-asd
      (:use :cl :asdf))
    ))
(in-package :kmp-asd)

(defsystem kmp
  :version "0.1"
  :author "Mike Bobak"
  :license "LLGPL?"
 ;:depends-on (:km)
 ;:depends-on (:ml)
  :components ((:module "src"
                :serial t
                ;(:file "owl2km") ;debug
                :components (
                ;(:file "kmp") ;defpackage kmp, &rest use it
                 (:file "km_2-5-33") 
                 (:file "u2") 
                 (:file "util_mb") 
                 ))))
