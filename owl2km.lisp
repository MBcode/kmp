;bobak@balisp.org simple owl2km /testing
(ql 's-xml)
;could look4part of lib that runs off hooks/when it sees a tag
(defun s-xml (fn)
  (s-xml:parse-xml-file fn))

(defvar *x* (s-xml "univ-bench.owl"))
(defvar *i* (s-xml "University0_0.owl"))

;for owl files will get sets of list
;when see |owl|:|Class|  sv 2nd as ?class
;when see just another list&it starts w/|rdfs|:|subClassOf| sv 3rd as ?super
;then use t.cl/now u2 fnc sv-super ?class ?super
(defun owl-cls-p (l) (eq (first-lv l) '|owl|:|Class|))
(defun lol-eq-p (lol e)  
  (let ((l (first-lv lol))) (when (listp l) (eq (first-lv l) e))))
(defun l-eq-p (l e)  
  (when (listp l) 
   ;(eq (first-lv l) e)
    (eq (second-lv l) e)
    ))
;(defun rdfs-sc-p (lol)  
;  (let ((l (first-lv lol))) (when (listp l) (eq (first-lv l) '|rdfs|:|subClassOf|))))
(defun rdfs-sc-p (lol) (lol-eq-p lol '|rdfs|:|subClassOf|))

;(trace owl-cls-p rdfs-sc-p sv-super)

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
(defun owl-inv-p (lol) (lol-eq-p lol '|owl|:|inverseOf|))
(defun rdfs-sprop-p (lol) (lol-eq-p lol '|rdfs|:|subPropertyOf|))
;add DatatypeProperty
(defun owl-dprop-p (lol) (lol-eq-p lol '|owl|:|DatatypeProperty|))
;(defun rdf-res-p (lol) (lol-eq-p lol '|rdf|:|resource|))
;(defun rdf-abt-p (lol) (lol-eq-p lol '|rdf|:|about|))
(defun rdf_res-p (l) (l-eq-p l '|rdf|:|resource|))
(defun rdf_abt-p (l) (l-eq-p l '|rdf|:|about|))
(defun member- (a l) (when (fulll l) (member a l)))
(defun rdf-res-p (l) (member- '|rdf|:|resource| l))
(defun rdf-abt-p (l) (member- '|rdf|:|about| l))

(defun g3v (l) (when (full l) (rm-pound (third-lv (caar l)))))
;(defun l3v (l) (when (full l) (rm-pound (last-lv l))))
(defun l3v (l) (when (full l) (rm-pound (last_lv l))))
(defun ll3v (l) (when (full l) (rm-pound (last_lv (first-lv l)))))

;Might load 'component' sub-slots, and use those:
;USER(2): (show- "Slot")
;(Slot has 
;  (subclasses (Relation
;               Property
;               KM-Slot-Group
;               Interface-Slot))
;  (superclasses (Thing))
;  (instances  (....))) 

;even though each lol small, might be better2have1fnc that trys all -p instead of collecting
; also now that loading instances, need to set many triples w/in one lol

(defun owl2km-prop (l)
  "s-xml lol w/class&super info"
  (let ((pp (collect-if #'owl-oprop-p l))
        (dp (collect-if #'rdfs-dmn-p l))
        (rp (collect-if #'rdfs-rng-p l))
        (iv (collect-if #'owl-inv-p l))
        (sp (collect-if #'rdfs-sprop-p l))
        (sd (collect-if #'owl-dprop-p l))
        (rs (collect-if #'rdf_res-p l))
        (at (collect-if #'rdf_abt-p l))
        (ff (first-lv (first-lv l))) ;ins
        )
    (when (and pp dp rp) ;think domain&ranger always given in these owl files
      (let ((prop (rm-pound (third-lv (first-lv pp))))
           ;(dmn (third-lv (caar dp)))
           ;(rng (third-lv (caar rp)))
            (dmn (g3v dp))
            (rng (g3v rp))
            (inv (g3v iv))
            (sprop (g3v sp))
            (dprop (g3v sd))
            )
        (format t "~%prop:~a has domain:~a and range:~a" prop dmn rng) ;for now,then set w/km fnc
        ;(sn  has (instance-of (slot))  (domain (Thing)) (range (Thing)))
        ;(sv-cls sn "slot") (sv sn "domain" ?d) ..
        ;(sv-cls prop "slot")
        (sv- prop "instance-of" "Slot") ;could also use: Relation or Property
       ;(sv- prop "domain" (rm-pound dmn))
       ;(sv- prop "range" (rm-pound rng))
        (sv- prop "domain" dmn)
        (sv- prop "range"  rng)
        ;check these:
        (when inv (sv- prop "inverseProperty"  inv))
        (when sprop (sv- sprop "instance-of" prop))
        (when dprop (sv- sprop "instance-of" prop))
    ))
    ;#+IGNORE
    (when ff
      (let (
            (res (ll3v rs))
            (abt (ll3v at))
            (i (symbol-name ff))
            )
        (when res (format t "~%ins:~a ~a" i res))
        (when res (sv i res))))
  ))

(defun second-if-space (s)
  (let ((sl (split s)))
    (or (second-lv sl) (first-lv sl))))

(defun owl2km-ins (l)
  (let (
        (rs (collect-if #'rdf-res-p l))
        (at (collect-if #'rdf-abt-p l))
       ;(rs (rdf-res-p l))
       ;(at (rdf-abt-p l))
        (f1 (first-lv l)) ;ins
        (ff (first-lv (first-lv l))) ;ins
        )
    ;when ff
    (if (or ff f1)
      (let (
           ;(res (g3v rs))
           ;(abt (g3v at))
            (res (ll3v rs))
            (abt (ll3v at))
            (i (symbol-name (or ff f1)))
            )
        (format t "~%~a," i)
        (when res ;(format t "~%ins:~a ~a" i res)
          (format t " ~a" res)
          (when (len-gt res 1)
            (sv i "resource" res)))
        (when abt ;(format t "~%ins2:~a ~a" i abt)
          (format t " ~a" abt)
          (when (len-gt abt 1)
            ;(sv i "about" abt)
           ;(sv-cls (underscore_  (clean-slash abt)) i)
            (sv-cls (second-if-space  (clean-slash abt)) i)
            ))
      )
      ;(format t "~%~a,~a full len:~a" i l1 (len l))
      (format t ",~a full len:~a" (first-lv l) (len l))
    )
  ))
;will also need2capture heirarchical properties
;((|owl|:|ObjectProperty| |rdf|:ID "headOf") (|rdfs|:|label| "is the head of")
; ((|rdfs|:|subPropertyOf| |rdf|:|resource| "#worksFor"))) 
 
;(trace owl-oprop-p rdfs-dmn-p rdfs-rng-p owl2km-prop)
; decide if a lol is cls or property, &just call the proper owl2km-... fnc
(defun owll2km (l)
  "sexp of owl elt to km elt"
  (let ((l1 (first-lv l)))
    (cond ((owl-cls-p l1) (owl2km-cls l))
          ((owl-oprop-p l1) (owl2km-prop l))
          (t ;(format t "~%~a full len:~a" l1 (len l))
              (owl2km-ins l)
              ))))

(defun owl2km (fn)
  "owl file2km assertions"
  (mapcar #'owll2km (s-xml fn)))

;test fncs,  load LUBM benchmark into KM, so can test queries
(defun t1 (&optional (f "univ-bench.owl"))
  "load http://swat.cse.lehigh.edu/onto/univ-bench.owl"
  (owl2km f))
(defun t2 (&optional (f "University0_0.owl"))
  "load http://swat.cse.lehigh.edu/projects/lubm/University0_0.owl"
  (owl2km f))
(defun t3 () 
  (format t "~%load defualt lubm ontology~%")
  (t1)
  (format t "~%load defualt lubm instances~%")
  (t2)
  (format t "~%lets see what it looks like:~%")
  (taxonomy))
