;play w/here before going to an asd file
;(lkm2) ;.sbclrc load of km&utils
(lkm3) ;lkm3 = lu+lkm2
;find my loadable alg3 http://www.cs.utexas.edu/users/qr/algy/download.html &try2work w/
(ql 'cl-store) ;don't use the one that comes w/mlcl
(al 'mlcl)
(load "arff.lisp" :print t) ;cl-arff-parser to km
(get-arff)

;need km components diretory for this:
(load "load-lib.lisp" :print t)
(load_lib) ;not working yet

(taxonomy)
