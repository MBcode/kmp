;a quick test file
(lkmq)  
(use-package :km) 
(get-all-symbols :km) ;should not need these
(import '(km::km-add-to-kb-object-list km::reset-kb km::put-list)) ;get more
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
