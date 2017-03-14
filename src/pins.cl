;Minipulate pins files

;defun read-preserve (s) ;now in util_mb
(defun read_preserve (s)
  (let ((rtc (readtable-case *readtable*)))
    (setf (readtable-case *readtable*) :preserve)
    (let ((l (read s nil nil)))
      (setf (readtable-case *readtable*) rtc)
      l)))

;need primary-key slot for every class, usually classnameID
;other refs should be other clsnameID
;|[<clsname>_Instance_<int>]| |of| |<clsname>|
  
(defun pins-pkval (ls)
  (let* ((cls (symbol-name (third ls)))
         (pkslt (sym-cat cls "." cls "ID"))
         (rl (subseq ls 3))
         (sv (second-lv (find pkslt rl :key #'first))))
    (when sv (sym-cat "[" cls "_Ins_" sv))))

(defun pins-w-pkId (ls)
  "rename ins w/primary key id"
  (let ((pkyval (when (full ls) (pins-pkval ls))))
    (when pkyval (setf (first ls) pkyval)))
  (print ls)) ;setup out file strm next

(defun pinsf-w-pkId (pf)
  "rename ins[name] in pins file w/primary key id"
  (with-open-file (strm pf :direction :input)
    (while (pins-w-pkid (read_preserve strm)))
  ))
