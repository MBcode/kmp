;load rdf/etc w/wilbur then assert to km; bobak@balisp.org
 
;from my .sbclrc:
(lu)  ;utils
(lkm2) ;km +utils
;libs:
(ql 'cl-ppcre)
(ql 'wilbur)  ;could also try cl-rdfxml

(defun sfs (s) 
 "safe symbol/node"
  (typecase s
        (symbol  (symbol-name s))
        (wilbur:node  (wilbur:node-uri s))  
        (t  s)))

;defun uri-end  (u)
(defun pound-on (u)
 "only part after the #"
  (let ((u2 (explode= u #\#)))
    (if (len-gt u2 1) (second u2) u)))
 
(defun uri-end (u)
 "after # or last / "
  (last_lv (explode= (pound-on u) #\/)))

(defun sfu (nd)
 "safe uri, get node name after #|/ "
  (uri-end (sfs nd)))

;later assert rest of url once under every new symbol
;  then deal w/collisions once that is a problem

;was just using sfs
(defun sbj (tr) (sfu (wilbur:triple-subject  tr)))
(defun prd (tr) (sfu (wilbur:triple-predicate  tr)))
(defun obj (tr) (sfu (wilbur:triple-object  tr)))

(defun p-spo (tr)
 (format t "~%s:~a p:~a o:~a~%" (sbj tr) (prd tr) (obj tr))) 

(defun a-spo (tr) ;get this going
  "km assert  triple"
 ;(assert  (:triple  (sbj tr) (prd tr) (obj tr)))
 ;(km-assert  (:triple  (sbj tr) (prd tr) (obj tr)))
 ;(ka (str-cat  "(:triple " (sbj tr) " " (prd tr) " " (obj tr) ")"))
 (let ((as (format nil "(:triple ~a ~a ~a)" (sbj tr) (prd tr) (obj tr))))
  (format t "~%will-assert:~a~%" as) ;dbg
   (ka as)
 ))

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
 ;fix/finish these,  both ~work now
 (a-spo (first *tr*))
 (s-spo (first *tr*))
 ;mapcar #'.-spo *tr*    ;brings out other val probs2fix soon
)
