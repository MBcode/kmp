;Minipulate protege pins files ;mike.bobak@gmail
; have datamaster imports that do not use primary key
 ;which I would like to use in the instance names

;{read_|write_|print_}preserve (s) ;in util_mb

;need primary-key slot for every class, usually classnameID

;need primary-key slot for every class, usually classnameID
;other refs should be other clsnameID
;|[<clsname>_Instance_<int>]| |of| |<clsname>|

(defun mc2sn (l) 
  "map 2 symbol-name" ;not used
  (cond ((symbolp l) (symbol-name l)) 
        ((fulll l) (mapcar #'mc2sn l)) 
        (t l))) ;then 'write' it

;could create both for pins&km below &/or also be able to convert pins->km
(defun bracket2star (s)
  "[insname]->*kmInsName" ;assume on symbols right now
 (if (not (symbolp s)) s 
  (let ((st (symbol-name s)))
    (when (prefixp "[" st) 
      (let ((nst (str-cat "*" (butlast- (subseq st 1)))))
        (if (stringp nst) ;(setf (symbol-name s) nst)
          (intern nst)
          s
          ))))))
(defun pins2sval (ls)
  "pIns's slot-val alist"
  (subseq ls 3))
(defun psv2ksv (al)
  "pins->km for (sn val)"
  (let ((sn (first al))
        (val (second al))) ;.  
    (list sn (bracket2star val))))
;also change 1st 3 in instance clp->km
;([insname] of class ..) -> (*insname has (instance-of (class)) ..)
(defun pins2km (ls) ;on /ins basis 1st
  (when (len-gt ls 4)
    (let ((insname (first ls))
          (cls (third ls))
          (rl (pins2sval ls))) ;skip 'ins of class' part, to process (sn val) list 
      (list (bracket2star insname) '|has| `(instance-of (,cls)) (mapcar ;#'(lambda (s v) (list s (bracket2star v))) 
                                                                  #'psv2ksv rl)))))
  ;test&improve soon


;In case primary key doesn't follow one standard
(load "cls2pkslt.l" :print t) ;has *cls2pkslt* & *ctlist*
;or: (defvar *cls2pkslt*  nil) (defvar *ctlist*)
(defun cls2pkslt (cls)
  "classname 2 primary key slot name"
  (let ((pkslt (sym-cat cls "." cls "ID"))
        (lk (assoc_v cls *cls2pkslt*)))
   ;(or lk pkslt)
    (if lk (sym-cat cls "." lk) pkslt)
    ))
  
(defun pins-pkval (ls)
  "if (primary_key pk) in ins, rename it as [Class_pk]"
  (let* (;(cls (symbol-name (third ls)))
         (cls (third ls))
        ;(pkslt (sym-cat cls "." cls "ID"))
         (pkslt (cls2pkslt cls))
         (rl (subseq ls 3)) ;skip 'ins of class' part, to process (sn val) list
         (sv (second-lv (find pkslt rl :key #'first)))) ;#
    (when sv (sym-cat "[" cls "_Ins_" sv "]"))))
;at some point want other keys to ref the class type but
 ;can do w/a method if all the ins have consistent naming
  ;this is for all *ID s that have just 1 ins w/that #
;-
(defun clsins-ref (cls n &optional (strm t)) (format strm "(~a [~a_Ins~d])" cls cls n))
(defun clsins_ref (cls n) (list cls (sym-cat "[" cls "_Ins_" n "]")))
(defun clsins_ref-km (cls n) (list cls (sym-cat "*" cls "_Ins_" n)))
;defun id2clsins-ref (l2 &optional (strm t)) ;works w/either version of clsins*ref: 
(defun id2clsins_ref (l2)  ;ins of c=should=c.sn 
  "if (..ID #) create insref: (.._ [])  else nil" ;assume sn is cls.sn
 (when (consp l2)
  (let* ((fsn (to-str (first l2)))
         ;(sn (second-lv (explode- fsn #\.)))
         (c+sn (explode- fsn #\.))
         (c (first-lv c+sn));make it not add if new cr is same as original cls
         (sn (second-lv c+sn))
         (cr (when sn (car-lv (rassoc (intern sn) *cls2pkslt*)))) ;..ID -> cls to ref2
         (n (first-lv (cdr l2)))) 
    (when (and cr (not (eq cr (intern c)))) (clsins_ref cr n)))))
;-

(defun pins-w-pkId (ls &optional (os nil))
  "rename ins w/primary key id" ;might need2keys,;todo/?
 (when ls
  (let ((pkyval (when (full ls) (pins-pkval ls)))) ;or use fulll ?
    (when pkyval (setf (first ls) pkyval))) ;reset
  ;-need to (mapcar #"id2clsins-ref ls) ;&append before the print
  (let* ((new (rm-nil (mapcar #'id2clsins_ref ls)))
         (ls2 (when (fulll new) (append ls new))))
    (if os ;(write (mc2sn ls) :stream os) ;out file strm 
           (print_preserve ls2 os)
           (print ls2)))))

(defun pinsf-w-pkId (pf &optional (of nil))
  "rename ins[name] in pins file w/primary key id"
  (with-open-file (strm pf :direction :input)
    (if of (with-open-file (ostrm of :direction :output :if-exists :supersede)
             (while (pins-w-pkid (read_preserve strm) ostrm)))
      (while (pins-w-pkid (read_preserve strm))))))

(defun pins-p2pl (fb)
  "for each class/table name"
  (pinsf-w-pkId (str-cat "p/" fb ".pins") 
                (str-cat "pl/" fb ".pins")))

(defun p2pln (&optional (n 0))
  (mapcar #'pins-p2pl (subseq *ctlist* n)))
