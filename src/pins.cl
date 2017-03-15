;Minipulate protege pins files ;mike.bobak@gmail
; have datamaster imports that do not use primary key
 ;which I would like to use in the instance names

;defun read-preserve (s) ;now in util_mb
(defun read_preserve (s)
  (let ((rtc (readtable-case *readtable*)))
    (setf (readtable-case *readtable*) :preserve)
    (let ((l (read s nil nil)))
      (setf (readtable-case *readtable*) rtc)
      l)))

(defun write_preserve (o s)
  (let ((rtc (readtable-case *readtable*)))
    (setf (readtable-case *readtable*) :preserve)
    (let ((l (write o :stream s)))
      (setf (readtable-case *readtable*) rtc)
      l)))

;need primary-key slot for every class, usually classnameID

;need primary-key slot for every class, usually classnameID
;other refs should be other clsnameID
;|[<clsname>_Instance_<int>]| |of| |<clsname>|

(defun mc2sn (l) 
  "map 2 symbol-name"
  (cond ((symbolp l) (symbol-name l)) 
        ((fulll l) (mapcar #'mc2sn l)) 
        (t l))) ;then 'write' it

;In case primary key doesn't follow one standard
(load "cls2pkslt.l" :print t) ;has *cls2pkslt*
(defun cls2pkslt (cls)
  "classname 2 primary key slot name"
  (let ((pkslt (sym-cat cls "." cls "ID"))
       ;(lk (when (boundp *cls2pkslt*) (assoc_v cls *cls2pkslt*)))
        (lk (assoc_v cls *cls2pkslt*)))
   ;(or lk pkslt)
    (if lk lk pkslt)
    ))
  
(defun pins-pkval (ls)
  (let* (;(cls (symbol-name (third ls)))
         (cls (third ls))
        ;(pkslt (sym-cat cls "." cls "ID"))
         (pkslt (cls2pkslt cls))
         (rl (subseq ls 3))
         (sv (second-lv (find pkslt rl :key #'first)))) ;#
    (when sv (sym-cat "[" cls "_Ins_" sv "]"))))
;at some point want other keys to ref the class type but
 ;can do w/a method if all the ins have consistent naming
  ;this is for all *ID s that have just 1 ins w/that #

(defun pins-w-pkId (ls &optional (os nil))
  "rename ins w/primary key id"
  (let ((pkyval (when (full ls) (pins-pkval ls))))
    (when pkyval (setf (first ls) pkyval)))
  (if os ;(write (mc2sn ls) :stream os) ;out file strm 
         (write_preserve ls os)
         (print ls))
  ;(format os "~%") ;(write-char #\newline os) ;fix newline for file
  )

(defun pinsf-w-pkId (pf &optional (of nil))
  "rename ins[name] in pins file w/primary key id"
  (with-open-file (strm pf :direction :input)
    (if of (with-open-file (ostrm of :direction :output :if-exists :supersede)
             (while (pins-w-pkid (read_preserve strm) ostrm)))
      (while (pins-w-pkid (read_preserve strm))))))
