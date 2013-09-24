(lkm2) ;.sbclrc load of km&utils
;find my loadable alg3 http://www.cs.utexas.edu/users/qr/algy/download.html &try2work w/
(ql 'cl-store) ;don't use the one that comes w/mlcl
(al 'mlcl)

;need km components diretory for this:
(load "load-lib.lisp" :print t)
(load_lib)
