(al 'cl-arff-parser)
(use-package :cl-arff-parser)
;(pprint (parse-arff "example.arff"))
;(defvar *i* (parse-arff "example.arff")) 
 
(defun get-arff (&optional (fn "example.arff")) 
  (let* ((ains (parse-arff fn))
         (cls (first (split-by #\. fn)))
         (attr (arff-attributes ains)))
    ;(sv-cls cls Thing) ;but not as an is,as subclass
    (sv-cls fn cls)
    (sv-al fn attr) ;but need2turn2alst 1st /so not so embedded
    ))

(trace get-arff)
