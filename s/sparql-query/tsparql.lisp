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
  (tshell-cmnd-sxml (str-cat str fn)))

;now have a template query, &parts to change
; that will go into this tmp file, and be processed
;   also look for other outputs
(defvar *tc* "select distinct ?C where { ?C rdf:type dbo:Organisation.  ?C rdfs:label ?title . ")
(defvar *ffs* "FILTER REGEX(?title, \"~a\", \"i\")")
;}
(defun tc (fl &optional (tc *tc*)) 
  ;loop for fs in fl collect  
  (str-cat tc
           (mapcar #'(lambda (f) (format nil *ffs* f)) fl)
           "}")
  )
