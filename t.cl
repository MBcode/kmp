;a quick test file
(lkmq) ;(lkm3) 
(use-package :km) 
(get-all-symbols :km) ;should not need these
(import '(km::km-add-to-kb-object-list km::reset-kb km::put-list
          km::taxonomy km::fastload-kb km::fastsave-kb)) ;get more
(defun tax (&optional (c))  ;though I already had this
  (if c (km::taxonomy c)
    (km::taxonomy)))
(defun fl (fn) (km::fastload-kb fn))
;fl a.kb then tax to see that it is there 

;use:
;USER(1): (tax)
;Removing redundant superclasses...
;Removing redundant subclasses...
;Computing subclasses of Thing...
;Thing
;(KM::|t|)
;USER(2): (sv-cls "s" "Thing")
;
;ka:(*s has (instance-of (Thing))),CONS
;KM::|*s|
;USER(3): (sv "s" "p" "o")
;
;ka:(*s has (p ("o"))),CONS
;KM::|*s|
;USER(4): (show "s")
;(*s has 
;  (instance-of (Thing))
;  (p ("o")))
;
;(KM::|*s|)
;USER(5): (gv "s" "p")
;
;ka:(the p of *s),CONS
;"o"
;USER(6): (tax)
;Removing redundant superclasses...
;Removing redundant subclasses...
;Computing subclasses of Thing...
;Thing
;?  *s
;(KM::|t|)
;USER(7): 

;vivacegraph-v3 agraph cl-neo4j
 ;rDB2graph, /r2rml ;then qry..
;want foriegn-keys 2be link/relations automatically
 ;and to add to this incrementally afterwards
 ;starting from a mysqldump &quickly loading
;might use clsql-orm or similar as well
;For Protege in java https://github.com/phillord/protege-nrepl /clojure-tab but not really going
; had heard of abcl as a tab, but he never packaged it up; some lisp but not better than my clp code yet
