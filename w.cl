(lu)
(lkm2)
(ql 'cl-ppcre)
(ql 'wilbur)  ;could also try cl-rdfxml
(defun p-spo (tr)
 (format t "~%s:~a p:~a o:~a~%" (triple-subject tr) (triple-predicate tr) (triple-object tr))) 

(defun a-spo (tr) ;get this going
  "km assert"
 (assert  (:triple) (triple-subject tr) (triple-predicate tr) (triple-object tr))) 
