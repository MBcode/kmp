;bobak@balisp.org load schema.org
;(lkm2)  ;inculdes u2
(ql 'cl-json)
;(ql 'cl-json.test)
;(defvar *a* (json-test:decode-file "all.json"))

;(load "tnc.cl" :print t) ;https://github.com/MBcode/kmp/blob/master/src/tnc.cl
(defun decode-file (path) ;from tnc
  (with-open-file (stream path :direction :input)
    (json:decode-json-strict stream)))
#+ignore ;from cl-json.test
(defun decode-file (path)
  (with-open-file (stream path :direction :input)
    (with-fp-overflow-handler (invoke-restart 'placeholder :infty)
      (with-no-char-handler (invoke-restart 'substitute-char #\?)
        (json:decode-json-strict stream))))) 

(defvar *a* (decode-file "all.json")) ;http://schema.rdfs.org/all.json

;then use
;src/u2.lisp:(defun json2ins (n)
