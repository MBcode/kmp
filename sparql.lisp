;https://github.com/daimrod/dbpedia-sparql.git
;(in-package #:dbpedia-sparql)

(defparameter *http-proxy-server* nil)  ; hostname only! no http://
(defparameter *http-proxy-port* 80)

;defun send-query (query)
(defun send-query (query &optional (where "http://dbpedia.org/sparql"))
  (drakma:http-request where ;"http://dbpedia.org/sparql"
                       :method :get
                       :parameters
                       (list (cons "query" query)
                             (cons "format" "json"))
                       :proxy (unless (null *http-proxy-server*)
                                (list *http-proxy-server*
                                      *http-proxy-port*))))

(defun json->list (json)
  (json:decode-json-from-string json))

(defun upper-case (string)
  (map 'string #'char-upcase string))

(defun explode-by-slash (s) (explode- s #\/))

(defun url-end (s) (last_lv (explode-by-slash s)))
(defun url-end2 (s) (last (explode-by-slash s) 2))

(defun make-better-list (results)
  (let (titles)
    (cons
     (setf titles
           (loop :for title :in (alexandria:assoc-value (alexandria:assoc-value results :HEAD)
                                             :VARS)
                 :collect title))
     (loop :for line :in (alexandria:assoc-value (alexandria:assoc-value results :RESULTS)
                                      :BINDINGS)
           :collect
           (loop :for title :in titles
                 :collect (alexandria:assoc-value (alexandria:assoc-value line
                                                    (intern
                                                     (upper-case title)
                                                     :keyword))
                                       :VALUE))))))

;next send the where: (str-cat "http://" cls "/sparql")
(defun query->list (query)
  (multiple-value-bind (ret code)
      (send-query query)
    (case code
      (200 (make-better-list
            (json->list
             (babel:octets-to-string ret))))
      (t ret))))
 
(defun sparql2lst (query)
  (let ((r (query->list query)))
    (when (listp r)
      (mapcar #'(lambda (tri) (mapcar #'url-end2 tri)) r))))

(defun sparql2ins (query)
  (let ((rl (sparql2lst query)))
    (when (len_gt rl 1)
      (let ((f (first rl)) ;like csv header of varnames
            (r (rest rl)))  ;all the values
        ;assuming 1st var is the ins/name ;for now
        (let ((snl (mapcar #'last_lv (rest f))) ;varlist
              (cls "dbpedia.org"))
          (mapcar #'(lambda (vl)  (let ((i (ki_ (first-lv vl))))
                      (sv-cls i cls)
                      (mapcar #'(lambda (sn sv) (when (and i sn sv) (sv i sn sv)) )
                                         snl (mapcar #'last_lv (rest vl)))))
                  r))))))

(defun ti1 () (sparql2ins (read-file-to-string "src/i1.txt")))
(defun ti2 () (sparql2ins (read-file-to-string "src/i2.txt")))
(defun teo () (sparql2ins (read-file-to-string "src/eo.txt")))
;----might try some sicl
(defun nocom (sline) (unless (prefixp "#" sline) sline))
(defun read-file-to-string2 (fn &optional (filtfn #'nocom)) 
  (apply #'str-cat (rm-nils (map-lines fn filtfn))))
(defun tst (p) (sicl:parse-sparql (read-file-to-string (str-cat "tst/" p ".rq"))))
(defun tst2 (p) (sicl:parse-sparql (read-file-to-string2 (str-cat "tst/" p ".rq"))))
(defun t1 () (tst "syntax-sparql3/syn-01b"))
(defun t2 () (tst2 "syntax-sparql3/syn-01")) ;can stip comments now
(defun tn (n) (tst2 (str-cat "syntax-sparql3/syn-0" n))) ;extra sicl 
(defun tqn (&optional (n 2)) (tst (str-cat "lubm/q" n)))
