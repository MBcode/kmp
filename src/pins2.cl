;Minipulate protege pins files ;mike.bobak@gmail
;Started w/pins.cl which had to guess the foreign key info.
;pins2.cl has new info:
;Given a csv of fk_table/class, fk_col/slot, pk_table/class, pk_col/slot    rename the pins, and connect them
;Each row/pins gets has a (pk_class.pk_slot val) to be used to name the row/ins: pk_class_Ins_val
;If a pins has a (fk_class.fk_slot val) create a has-ins ref:         fk_class_  pk_class_Ins_val

;Since I might do more than I want to load, have considered dumping in dirs by table/cls name w/val.txt names
 ;this would probably more to be used by the KM version, and could be code&interlinked html for viewing as well.

; have datamaster imports that do not use foreign/primary key info, even though a place for this
;  Could try to use the meta slots, but I'm not sure that would allow for the easy linked ins viewing in protege

;{read_|write_|print_}preserve (s) ;in util_mb

;need primary-key slot for every class, usually classnameID <-use as default if don't see table/class in pk_table/class column

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
    ;when (prefixp "[" st) 
    (if (not (prefixp "[" st)) s  ;non insname sym pass through
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
    (list sn (list ; (val) 
               (bracket2star val)))))
;also change 1st 3 in instance clp->km
;([insname] of class ..) -> (*insname has (instance-of (class)) ..)
(defun pins2km (ls) ;on /ins basis 1st
  "1pins->1km ins"
  (when (len-gt ls 4)
    (let ((insname (first ls))
          (cls (third ls))
          (rl (pins2sval ls))) ;skip 'ins of class' part, to process (sn val) list 
      (append (list (bracket2star insname) '|has| `(instance-of (,cls))) (mapcar #'psv2ksv rl)))))
  ;test&improve soon

;=Consider reading the sql schema files (use a quicklisp, lib),&pulling out as much key info as possible. If re pk >1 plc, cnct.
;(ql 'crane) or (ql 'clsql-orm) or (ql 'pgloader) .. https://github.com/fukamachi/mito.git  
;doesn't get over loss of foreign-keys nor maybe sometimes defining more primary keys than necessary; but maybe ways around this
;incl looking@the data,2see what is really minimaly needed to ID a row in table, then make that the instance name in a ORMapping
;=google: infer foreign-key from primary-keys; has a few things to look at. ;look @maana.io 
;In case primary key doesn't follow one standard
;(load "cls2pkslt.l" :print t) ;has *cls2pkslt* & *ctlist*
;want to get rid of this, and instead load a file w/fk_table,fk_column,pk_table,pk_column and create the cached data-structs,but
(load "cls2pkslt.cl" :print t) ;for now ;has *cls2pkslt* but not *ctlist* ;could be skipped as make calcs from csv file below
;could skip load&reconstruct from *csv* below, but might shelve this for now, as have2go for much shorter/lesser wins right now
;(defvar *ctlist* (mapcar #'first *cls2pkslt*)) ;for now
 ;still seeing what are probably composite(primary)keys, which I guess I like more than the generated ID#s
   ;anyway this is causing hand editing of this file, so not fully automated from csv /yet ;still probably better2use csv
 ;Try to decide what to do w/multi PKs
;(defvar *csv* (read-csv "fktcpktc.csv"))
(defvar *csv* (rest (read-csv "fktcpktc.csv"))) ;get rid of header
;(defvar *ctlist* (remove-duplicates (mapcar #'first *csv*) :test #'equalp)) ;for now
(load "ctlist.l") ;just to be able to hand edit for now
(defvar *pkc* (mapcar #'third *csv*)) ;cls of FK name can be diff from PK cls
(defvar *pk2* (mapcar #'(lambda (l) (sym-cat (third l) "." (fourth l))) (rest *csv*)))
(defvar *pku* (remove-duplicates *pk2* :test #'eq))
(defvar *fk2* (mapcar #'(lambda (l) (sym-cat (first l) "." (second l))) (rest *csv*)))
(defvar *fku* (remove-duplicates *fk2* :test #'eq))
(defun pk-p (k) "primary-key" (member k *pku*))
(defun fk-p (k) "foreign-key" (member k *fku*))
(defvar *fk2pk* (mapcar #'cons *fk2* *pk2*))
(defvar *fk2pkc* (mapcar #'list *fk2* *pk2* *pkc*))
;or: (defvar *cls2pkslt*  nil) (defvar *ctlist*)
;try: (mapcar #'(lambda (pr) (format t "~%~a -> ~a" (cdr pr) (car pr))) *cls2pkslt* ) to view it as a pk as fk -> table diagram
; but since I use it as the 1st occurance of the ID, &this has multiple, I'd have to limit diag; so collect cdr|uniq,then map rassoc
; I should probably just do that once, and have a 2nd alst for that one rev lookup, that is easier to debug
;Would also be nice to have lsp code to automate even the defvar creation if the ID naming was completely consistent, but not always
;-
;Could write a fnc to rev a alst, and only take the 1st occurance of the val as a key 
(defun rev-alst (al)
  "reverse an assoc list"
  (let ((vakl nil)) ;value as key list ;only use 1st occurance, so keep a stack that can be checked
   (rm-nils
    (mapcar #'(lambda (pr) (let ((k2v (car pr)) 
                                 (v2k (cdr pr)))
                             (when (not (member v2k vakl)) (cons v2k k2v)))) al))))
(defun mk-pk2cls ()
  (rev-alst *cls2pkslt*))
(defvar *pk2cls* (mk-pk2cls))
  ;=see if this is where I can insert the new more certain fk_ info=
;-
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
(defun underscore- (s) (if (stringp s) (safe-trim (underscore s)) s))
;actually might need a safe string like this ;hoped this val would just be a number
;-
(defun clsins-ref (cls n &optional (strm t)) (format strm "(~a [~a_Ins~d])" cls cls n))
(defun clsins_ref (cls n) (list cls (sym-cat "[" cls "_Ins_" n "]")))
(defun clsins_ref_ (cls n) (list (sym-cat cls "_") (sym-cat "[" cls "_Ins_" (underscore- n) "]"))) ;pont should have ins refs: cls_
(defun clsins_ref-km (cls n) (list cls (sym-cat "*" cls "_Ins_" n)))
;defun id2clsins-ref (l2 &optional (strm t)) ;works w/either version of clsins*ref: 
(defun id2clsins_ref (l2)  ;ins of c=should=c.sn 
  "if (..ID #) create insref: (.._ [])  else nil" ;assume sn is cls.sn
  ;if fk-p sn  (sltname val) where sltname=cls.sn, -add-> (cls_ cls_Ins_val) ;clsins_ref
 (when (consp l2) (let ((sltname (first l2))) (when (fk-p sltname) ;later assoc w/known pk, for now..;well diff cls so..
  (let* ((fsn (to-str sltname ;(first l2)
                      ))
         (pkc (last_lv (assoc sltname *fk2pkc*))) ;cls of FK name can be diff from PK cls
         ;(sn (second-lv (explode- fsn #\.)))
         (c+sn (explode- fsn #\.))
         (c (first-lv c+sn));make it not add if new cr is same as original cls
         (sn (second-lv c+sn))
         (cr (when sn ;(car-lv (rassoc (intern sn) *cls2pkslt*)) ;..ID -> cls to ref2
                       (assoc_v (intern sn) *pk2cls*)))  ;But will this be mixed-case check-here/debug
         ;=this is where can use FK info=
         (n (first-lv (cdr l2))))  ;val
    (when (and cr (not (eq cr (intern c)))) ;(clsins_ref cr n)
     ;(clsins_ref (intern pkc) n)
      (clsins_ref_ pkc n)
      )))))) ;should be using this
;-consider falling back2old pins.cl behavior if the csv file doesn't hold enough info

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

(defun pins2km-o (ls &optional (os nil))
  "sexpr -pins2km-> os file"
  (let ((ls2 (pins2km ls)))
    (if os ;(write (mc2sn ls) :stream os) ;out file strm 
           (print_preserve ls2 os)
           (print ls2))))

(defun read-sexprs+transform (in fnc &optional (of nil))
  "in file transformed w/fnc to 'of' file"
  (with-open-file (strm in :direction :input)
    (if of (with-open-file (ostrm of :direction :output :if-exists :supersede)
             (while (funcall fnc (read_preserve strm) ostrm)))
      (while (funcall fnc (read_preserve strm))))))

(defun pinsf-w-pkId (in &optional (of nil))
  "pins in file to 'of' transformed output file"
    (read-sexprs+transform in #'pins-w-pkid of))

(defun pins2km-io (in &optional (of nil))
  "pins in file to 'of' output KM file"
    (read-sexprs+transform in #'pins2km-o of))

;Generalizing other fncs above but in a more compact way
(defun pinsf-w-pkId- (pf &optional (of nil)) ;really check that I shouldn't still be using this
  "rename ins[name] in pins file w/primary key id"
  (with-open-file (strm pf :direction :input)
    (if of (with-open-file (ostrm of :direction :output :if-exists :supersede)
             (while (pins-w-pkid (read_preserve strm) ostrm)))
      (while (pins-w-pkid (read_preserve strm))))))

(defun pins-p2pl (fb)
  "for each class/table name, transform2 pk name/pnt-ing"
  (pinsf-w-pkId (str-cat "p/" fb ".pins") 
                (str-cat "pl/" fb ".pins")))

(defun pins-pl2km (fb)  ;after pl/ full off pins files, can transform to pk/*.km files
  "for each class-file pins->km"
  (pins2km-io (str-cat "pl/" fb ".pins") 
              (str-cat "pk/" fb ".km")))

;-transform all class pins files to PrimaryKey nameing&add(new)slots to point to these names
(defun p2pln (&optional (n 0))
  (mapcar #'pins-p2pl (subseq *ctlist* n)))

(defun pl2pkn (&optional (n 0))
  (mapcar #'pins-pl2km (subseq *ctlist* n)))

(defun pins2+km (&optional (n 0))
  (p2pln n)
  (pl2pkn n))
