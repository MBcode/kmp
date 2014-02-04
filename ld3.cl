;(al 'cl-kb)
(al 'cl-kb) ;try loading:
(setf *protege-ex-01*  "/Users/bobak/dwn/ai/ont/prot/Protege_3.5/gsdml.pprj") 
(lkm3)
;(load "xml.cl" :print t)
(load "owl2km.lisp" :print t)
;(lt) ;addition in kmb/u2.lisp now
(t3) ;load LUBM
 
;next
(load-kb "cube.km") ;datacube ontology
;and:
(load "load-lib.lisp" :print t)
(load_lib) ;km core component ontology
(taxonomy) 
