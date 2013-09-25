;bobak@balisp.org simple owl2km /testing
(ql 's-xml)
(defun s-xml (fn)
  (s-xml:parse-xml-file fn))

(defvar *x* (s-xml "univ-bench.owl"))

;for owl files will get sets of list
;when see |owl|:|Class|  sv 2nd as ?class
;when see just another list&it starts w/|rdfs|:|subClassOf| sv 3rd as ?super
;then use t.cl/now u2 fnc sv-super ?class ?super
(defun owl-cls-p (l) (eq (first-lv l) '|owl|:|Class|))
(defun rdfs-sc-p (lol)  
  (let ((l (first-lv lol))) (when (listp l) (eq (first-lv l) '|rdfs|:|subClassOf|))))

(trace owl-cls-p rdfs-sc-p sv-super)

(defun owl2km-cls (l)
  "s-xml lol w/class&super info"
  (let ((cp (collect-if #'owl-cls-p l))
        (sp (collect-if #'rdfs-sc-p l)))
    (when (and cp sp)
      (let ((cls (third-lv (first-lv cp)))
            (scl (third-lv (caar sp))))
         ; (sv-super scl cls)
        (when (stringp scl) (sv-super cls (rm-str "#" scl)))
          ))))
;USER(1): (sv-super "data" "Thing")
;USER(2): (show- "data")
;(data has 
;  (superclasses (Thing)))
;USER(3): (taxonomy)
;Thing
;   data
