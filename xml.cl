;bobak@balisp.org simple owl2km /testing
(ql 's-xml)
(defun s-xml (fn)
  (s-xml:parse-xml-file fn))

(defvar *x* (s-xml "univ-bench.owl"))

;for owl files will get sets of list
;when see |owl|:|Class|  sv 2nd as ?class
;when see just another list&it starts w/|rdfs|:|subClassOf| sv 3rd as ?super
;then use t.cl/now u2 fnc sv-super ?class ?super
;
;USER(1): (sv-super "data" "Thing")
;USER(2): (show- "data")
;(data has 
;  (superclasses (Thing)))
;USER(3): (taxonomy)
;Thing
;   data
