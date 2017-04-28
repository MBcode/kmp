;mike.bobak@gmail
;take protege pins file, and create fact sexpr-s ready for a lisa.sf.net assert
; might ready instances later, for assert &/or other uses ;&/or KM ins
;will be using:
;(ql 'lisa)
;(in-package :lisa)
;(make-inference-engine)

;msc fncs
;(defun neq2 (a b) (not (eq a b)))
;(defun neq-sp (a) (neq2 a ""))
;(import 'user::split)

;more clips like
(defun slot-put-value (ins sn val) (setf (slot-value ins sn) val)) 
;(import 'user::printout)
(defvar crlf "
  ")

(defun bt () (sb-debug:print-backtrace))
;write some code like pins*.cl that does a ins/fact assert for lisa.sf.net
;I also have some global hashing of instances code to be more clips like
; which would make using the foreign key code more useful
;For now skip that&just assert the (classname (sn sv) ...)

(defun read-sexprs+transform (in fnc &optional (of nil))
  "in file transformed w/fnc to 'of' file"
  (with-open-file (strm in :direction :input)
    (if of (with-open-file (ostrm of :direction :output :if-exists :supersede)
             (while (funcall fnc (read_preserve strm) ostrm)))
      (while (funcall fnc (read_preserve strm))))))

;defun pinsf-w-pkId (in &optional (of nil))
; "pins in file to 'of' transformed output file"
;   (read-sexprs+transform in #'pins-w-pkid of))
;defun pins-pkval (ls)
(defun pins-af (ls)
  "pins ins2 lisa fact for assertion: (classname (sn sv) ....)"
 (when (len-gt ls 3)
  (let* (;(cls (symbol-name (third ls)))
         (cls (third ls))
       ;;(pkslt (sym-cat cls "." cls "ID"))
       ; (pkslt (cls2pkslt cls))
         (rl (subseq ls 3)) ;skip 'ins of class' part, to process (sn val) list
       ; (sv (second-lv (find pkslt rl :key #'first))) ;#
        )
    ;when sv (sym-cat "[" cls "_Ins_" sv "]"))))
    ;might add assert here soon to, &/or as an option
    (cons cls rl))))

(trace pins-af)

(defun pinsf-af (in &optional (of nil))
  "pins in file to 'of' transformed output facts file"
    (read-sexprs+transform in #'pins-af of))
