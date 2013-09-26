;bobak@balisp.org simple owl2km /testing
(ql 's-xml)
;could look4part of lib that runs off hooks/when it sees a tag
(defun s-xml (fn)
  (s-xml:parse-xml-file fn))

(defvar *x* (s-xml "univ-bench.owl"))

;for owl files will get sets of list
;when see |owl|:|Class|  sv 2nd as ?class
;when see just another list&it starts w/|rdfs|:|subClassOf| sv 3rd as ?super
;then use t.cl/now u2 fnc sv-super ?class ?super
(defun owl-cls-p (l) (eq (first-lv l) '|owl|:|Class|))
(defun lol-eq-p (lol e)  
  (let ((l (first-lv lol))) (when (listp l) (eq (first-lv l) e))))
;(defun rdfs-sc-p (lol)  
;  (let ((l (first-lv lol))) (when (listp l) (eq (first-lv l) '|rdfs|:|subClassOf|))))
(defun rdfs-sc-p (lol) (lol-eq-p lol '|rdfs|:|subClassOf|))

(trace owl-cls-p rdfs-sc-p sv-super)

(defun rm-pound (s) (rm-str "#" s))

(defun owl2km-cls (l)
  "s-xml lol w/class&super info"
  (let ((cp (collect-if #'owl-cls-p l))
        (sp (collect-if #'rdfs-sc-p l)))
    (when (and cp sp)
      (let ((cls (third-lv (first-lv cp)))
            (scl (third-lv (caar sp))))
       ;(when (stringp scl) (sv-super cls (rm-str "#" scl)))
        (when (stringp scl) (sv-super cls (rm-pound scl)))
          ))))
;USER(1): (sv-super "data" "Thing")
;USER(2): (show- "data")
;(data has 
;  (superclasses (Thing)))
;USER(3): (taxonomy)
;Thing
;   data
 
;Next come relations: |owl|:|ObjectProperty|
(defun owl-oprop-p (l) (eq (first-lv l) '|owl|:|ObjectProperty|))
(defun rdfs-dmn-p (lol) (lol-eq-p lol '|rdfs|:|domain|))
(defun rdfs-rng-p (lol) (lol-eq-p lol '|rdfs|:|range|))

(defun owl2km-prop (l)
  "s-xml lol w/class&super info"
  (let ((pp (collect-if #'owl-oprop-p l))
        (dp (collect-if #'rdfs-dmn-p l))
        (rp (collect-if #'rdfs-rng-p l)))
    (when (and pp dp rp)
      (let ((prop (third-lv (first-lv pp)))
            (dmn (third-lv (caar dp)))
            (rng (third-lv (caar rp))))
        (format t "~%prop:~a has domain:~a and range:~a" prop dmn rng) ;for now,then set w/km fnc
        ;(sn  has (instance-of (slot))  (domain (Thing)) (range (Thing)))
        ;(sv-cls sn "slot") (sv sn "domain" ?d) ..
        ;(sv-cls prop "slot")
        (sv- prop "instance-of" "slot")
        (sv- prop "domain" (rm-pound dmn))
        (sv- prop "range" (rm-pound rng))
))))
(trace owl-oprop-p rdfs-dmn-p rdfs-rng-p owl2km-prop)
; decide if a lol is cls or property, &just call the proper owl2km-... fnc
(defun owll2km (l)
  "sexp of owl elt to km elt"
  (let ((l1 (first-lv l)))
    (cond ((owl-cls-p l1) (owl2km-cls l))
          ((owl-oprop-p l1) (owl2km-prop l))
          (t  (format t "~%~a full len:~a" l1 (len l))))))

(defun owl2km (fn)
  "owl file2km assertions"
  (mapcar #'owll2km (s-xml fn)))

(defun t1 (&optional (f "univ-bench.owl"))
  (owl2km f))
