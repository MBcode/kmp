;look at sysML
;http://booksite.elsevier.com/9780123852069/air_compressor_example.mdzip
;cp Air\ Compressor-OMG\ Style_ReadOnly\ \(from\ Alan\).mdxml air.xml
(ql 's-xml)
(defvar *air*  (s-xml:parse-xml-file "air.xml")) 
(lu)

;USER(5): (len *air*)
;112
;USER(6): (mapcar #'len *air*)
;(29 1 13 10 12 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
; 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
; 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)
