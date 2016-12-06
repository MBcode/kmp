;get&compile: https://github.com/tialaramex/sparql-query   ;bobak@balisp.org
;(lu)
(ql 's-xml)
;;-had this, but trivial-shell is easier
;(defun run-xml (cmd &rest args)
;  "run-ext&prs ret xml2s-expr"
;  ;STRING-OUTPUT-STREAM {100AAC9DC1}> is not a character input stream
;  (let ((str (make-string-output-stream))) ;just tried
;      (sb-ext:run-program cmd args :search t :output str)
;              (s-xml:start-parse-xml str))) 
;;-or
;(require :trivial-shell)
;(defun tshell-command (str)
; (trivial-shell:shell-command (to-str str))) 
;I already load a 'tsh' fnc from my .sbclrc

(defun tshell-cmnd-sxml (str)
   (s-xml:parse-xml-string 
     (tsh ;tshell-command 
       str)))

(defvar *i2* "sparql-query -np http://dbpedia.org/sparql < i2.txt")
(defvar *sq-f* "sparql-query -np http://dbpedia.org/sparql <")

(defun t2 (&optional (str *i2*))
  "make sparql qry of dbpedia, and parse resulting xml into an s-exp"
  (tshell-cmnd-sxml str)) 

(defun tf (&optional (fn "i2.txt") (str *sq-f*))
  "shell-cmd < fn"
  (tshell-cmnd-sxml (str-cat str fn)))

;now have a template query, &parts to change
; that will go into this tmp file, and be processed
;   also look for other outputs
(defvar *tc* "select distinct ?C where { ?C rdf:type dbo:Organisation.  ?C rdfs:label ?title . ")
(defvar *ffs* "FILTER REGEX(?title, \"~a\", \"i\")")
;}
;fl==filter-list, used w/template to make a filtered query
(defun tc (fl &optional (tc *tc*)) 
  "create qry"
  ;loop for fs in fl collect  
  (when (full fl)
    (str-cat tc
           (mapcar #'(lambda (f) (format nil *ffs* f)) fl)
           "}")
  ))
(defun tcs (fl &optional (tc *tc*) (sf "t2.tmp")) 
  "create qry, &cache"
  (save-lines (append (list tc) 
           (mapcar #'(lambda (f) (format nil *ffs* f)) fl)
              '(" } LIMIT 10"))  sf))
(defun rtcs (fl)
  "run cached qry"
  (tcs fl)
  (tf "i2.tmp"))

(defun mk-tmp-fn (l) (str-cat "tmp/" (underscore_ (implode-l l)) ".tmp"))

(defun print2tmpl (fn l)
  (with-open-file (strm fn :direction :output :if-exists :supersede)
      (print l strm)))

(defun tsfl (fl)
  "use|mk cached qry and run"
  (let ((tf (mk-tmp-fn fl)))
    (unless (file-exists-p tf) (tcs fl *tc* tf))
    (tf tf)
    ))
(defun subseq-mx (s mx) (subseq s 0 (min mx (len s))))
(defun subseq-mx2 (s) (subseq-mx s 2))
(defun f2sq (fn) ;try: "c2o6.csv"
  "take 1st words of each line &qry for that org"
  ;let ((loqw (mapcar #'explode= (list-lines fn))))
  (let ((loqw (mapcar #'subseq-mx2 (mapcar #'explode= (list-lines fn)))))
    (mapcar #'tsfl loqw)))

;I should cache the returns as well, so I don't have to hit dbpedia during testing/reuse
 ;do as above or use a caching lib  ;might start w/just appending tf w/.out  or .l
   ;just need strs for save-lines or similar, a sv-sexp(print 2file strm) so could load again right away/find
 ;consider saving qry str in out file,(in case it changes/just4gen provenance)
;defun tsfl (fl)
(defun qfl (fl) ;qry orgs, w/filter list on parts of name
  "use|mk cached qry and run, &cache"
  (let* ((tf (mk-tmp-fn fl))
         (tfl (str-cat tf ".l")))
    (unless (file-exists-p tf) (tcs fl *tc* tf))
    (if (file-exists-p tfl) (list-lines tfl) ;finish
      (print2tmpl tfl (tf tf))) ;run-qry&cache
    ));next print.. (append qry-l resp-l)
(defun t2b (&optional (fl '("parc")))
   (qfl fl))
