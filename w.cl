;load rdf/etc w/wilbur then assert to km; bobak@balisp.org
(lu)
(lkm2)
(ql 'cl-ppcre)
(ql 'wilbur)  ;could also try cl-rdfxml
(defun sfs (s) 
  (typecase s
        (symbol  (symbol-name s))
        (wilbur:node  (wilbur:node s))  
        (t  s)))

(defun sbj (tr) (wilbur:triple-subject (sfs tr)))
(defun prd (tr) (wilbur:triple-predicate (sfs tr)))
(defun obj (tr) (wilbur:triple-object (sfs tr)))

(defun p-spo (tr)
 (format t "~%s:~a p:~a o:~a~%" (sbj tr) (prd tr) (obj tr))) 

(defun a-spo (tr) ;get this going
  "km assert"
;(assert  (:triple  (sbj tr) (prd tr) (obj tr)))
 (km-assert  (:triple  (sbj tr) (prd tr) (obj tr)))
 ) 

(defun s-spo (tr) ;get this going
  "km set"
 (sv (sbj tr) (prd tr) (obj tr))) 

(defvar wilbur:*db* nil)
(defvar *tr* nil)

(defun tdb ()
 "test &have globals around2play w/"
 (setq wilbur:*db*  (wilbur::make-instance 'wilbur:db))
 (wilbur:db-load wilbur:*db* "http://datagraph.org/jhacker/foaf.rdf")
 (wilbur:db-load wilbur:*db* "http://notional.no-ip.org/foaf.rdf")
 (setq *tr*  (wilbur:query !"http://datagraph.org/jhacker/#self" nil nil))
 (p-spo (first *tr*))
 ;s:!"http://datagraph.org/jhacker/#self" p:!rdf:type o:!foaf:Person
 ;fix/finish these
 (a-spo (first *tr*))
 (s-spo (first *tr*))
)
