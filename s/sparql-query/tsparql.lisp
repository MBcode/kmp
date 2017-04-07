;get&compile: https://github.com/tialaramex/sparql-query   ;bobak@balisp.org
;(lu)
(ql 's-xml)
;;-had this, but trivial-shell is easier
;(defun run-xml (cmd &rest args)
;  "run-ext&prs ret xml2s-expr"
;  ;STRING-OUTPUT-STREAM {100AAC9DC1}> is not a character input stream
;  (let ((str (make-string-output-stream))) ;just tried
;      (sb-ext:run-program cmd args :search t :output str)
;              (s-xml:start-parse-xml str))) 
;;-or
;(require :trivial-shell)
;(defun tshell-command (str)
; (trivial-shell:shell-command (to-str str))) 
;I already load a 'tsh' fnc from my .sbclrc

(defun tshell-cmnd-sxml (str)
   (s-xml:parse-xml-string 
     (tsh ;tshell-command 
       str)))

(defun fwdslash2hyphen (s) (substitute #\- #\/ s))
(defun explode-space (s) (explode-str s :sep #\Space))

(defvar *i2* "sparql-query -np http://dbpedia.org/sparql < i2.txt")
(defvar *sq-f* "sparql-query -np http://dbpedia.org/sparql <")

(defun t2 (&optional (str *i2*))
  "make sparql qry of dbpedia, and parse resulting xml into an s-exp"
  (tshell-cmnd-sxml str)) 

(defun tf (&optional (fn "i2.txt") (str *sq-f*))
  "shell-cmd < fn"
  (tshell-cmnd-sxml (str-cat str fn)))

;now have a template query, &parts to change
; that will go into this tmp file, and be processed
;   also look for other outputs
(defvar *tc* "select distinct ?C where { ?C rdf:type dbo:Organisation.  ?C rdfs:label ?title . ")
(defvar *ffs* "FILTER REGEX(?title, \"~a\", \"i\")")
;}
;fl==filter-list, used w/template to make a filtered query
(defun tc (fl &optional (tc *tc*)) 
  "create qry"
  ;loop for fs in fl collect  
  (when (full fl)
    (str-cat tc
           (mapcar #'(lambda (f) (format nil *ffs* f)) fl)
           "}")
  ))
(defvar *limit* 20) ;(defvar *qry-cap* (list (format nil " } LIMIT ~a" *limit*)))
(defun save-lines- (l filename)
   (when (fulll l)
       (with-open-file (stream filename :direction :output :if-exists :supersede)
             (mapcar #'(lambda (x) (write-line x stream)) l))))

(defun tcs (fl &optional (tc *tc*) (sf "t2.tmp")) 
  "create qry, &cache"
  ;save-lines (append (list tc) 
  (save-lines- (append (list tc) 
           (mapcar #'(lambda (f) (format nil *ffs* f)) fl)
              '(" } LIMIT 20"))  sf))
              ;   *qry-cap*
(defun rtcs (fl)
  "run cached qry"
  (tcs fl)
  (tf "i2.tmp"))

(defun mk-tmp-fn (l) (str-cat "tmp/" 
                          (fwdslash2hyphen
                              (underscore_ (implode-l l)))
                              ".tmp"))

(defun print2tmpl (fn l)
  (with-open-file (strm fn :direction :output :if-exists :supersede)
      (print l strm)))

(defun read-fn (fn)
  (with-open-file (strm fn :direction :input)
     ;(read strm) (READ-SEQUENCE strm)
     (read-from-string 
      (rm-strs '("NS-0:" "|xsi|:") ;from xml pkg used
       (read-file-to-string fn))
      )))

(defun tsfl (fl)
  "use|mk cached qry and run"
  (let ((tf (mk-tmp-fn fl)))
    (unless (file-exists-p tf) (tcs fl *tc* tf))
    (tf tf)
    ))
(defun subseq-mx (s mx) (subseq s 0 (min mx (len s))))
(defun subseq-mx2 (s) (subseq-mx s 2)) ;or just pass a lambda
(defun subseq-mx3 (s) (subseq-mx s 3))  ;or a 2nd arg
(defun f2sq (fn) ;try: "c2o6.csv"
  "take 1st words of each line &qry for that org"
  ;let ((loqw (mapcar #'explode= (list-lines fn))))
  (let ((loqw (mapcar #'subseq-mx2 (mapcar #'explode= (list-lines fn)))))
    (mapcar #'tsfl loqw)))

;I should cache the returns as well, so I don't have to hit dbpedia during testing/reuse
 ;do as above or use a caching lib  ;might start w/just appending tf w/.out  or .l
   ;just need strs for save-lines or similar, a sv-sexp(print 2file strm) so could load again right away/find
 ;consider saving qry str in out file,(in case it changes/just4gen provenance)
;defun tsfl (fl)
(defun qfl (fl) ;qry orgs, w/filter list on parts of name
  "qry w/filter-list, use|mk cached qry and run, &cache"
  (let* ((tf (mk-tmp-fn fl))
         (tfl (str-cat tf ".l")))
    (unless (file-exists-p tf) (tcs fl *tc* tf))
    (if (file-exists-p tfl) (read-fn tfl) ;(list-lines tfl) ;finish
      (print2tmpl tfl (tf tf))) ;run-qry&cache
    ));next print.. (append qry-l resp-l) ;Nope, save the qry in a sep tmp file

(defun t2b (&optional (fl '("parc")))
   (qfl fl))

;defun f2sq (fn) ;try: "c2o6.csv"
(defun fn2qfl (fn &optional (fflf #'subseq-mx2)) ;filter filter-list fnc
  "take 1st words of each line &qry for that org"
  ;let ((loqw (mapcar #'explode= (list-lines fn))))
  ;let ((loqw (mapcar #'subseq-mx2 (mapcar #'explode= (list-lines fn)))))
  (let ((loqw (mapcar fflf (mapcar #'explode= (list-lines fn))))) ;list of query words
    (mapcar #'qfl loqw)))

;If ret#hits==*limit* try up the filterList len(eg.subseq-mx3)and try again, assuming there is a 3rd word to use
;defun qfl (fl) ;qry orgs, w/filter list on parts of name
;defun qfl2 (fl flf2) ;qry orgs, w/filter list on parts of name; 
   ;let ((ret (qfl fn (funcall (first-lv fflfs)))))
 ;skip the fncs, and just have the two lengths of filter lists to try:
(defun qfl2 (fl &optional (fll2 '(2 3)));qry orgs,w/filter l on parts of name; filter-list-len-s2 
  "qry w/filter list, but start smaller &incr if useful"
   (let* (;(fll (len fl)) ;filter-list len
          (fl2u (first-lv fll2)) ;filter-list len to use
          (ret (qfl (subseq-mx fl (first-lv fll2)))))
     ;if (and (len-gt ret (* 0.75 *limit*)) (len-gt fl 1)) ;this assumes kn of fll2 lens /fix
     (if (and (len-gt ret (* 0.75 *limit*)) (len-gt fl fl2u)) ;room2incr len of fl
       (qfl (subseq-mx fl (second-lv fll2)))
       ret)))
(trace qfl)

;defun fn2qfl (fn &optional (fflf #'subseq-mx2)) ;filter filter-list fnc
;defun fn2qfl2 (fn &optional (fflf #'subseq-mx2)) ;filter filter-list fnc
;defun fn2qfl2 (fn &optional (fflfs '(#'subseq-mx2 #'subseq-mx3))) ;filter filter-list functions
(defun fn2qfl2 (fn) ;csv filename
  ;let ((loqw (mapcar fflf (mapcar #'explode= (list-lines fn))))) ;list of query words
  (let ((loqw (mapcar #'explode= (list-lines fn)))) ;list of query words, resizing done in qfl2
    (mapcar #'qfl2 loqw)))  ;skip sending in opt, just go for defaults right now

(defun try2findOrgs (csvFN) (fn2qfl2 csvFN)) ;use this

;defun fn2qfl2 (fn &optional (fflfs '(#'subseq-mx2 #'subseq-mx3))) ;filter filter-list functions
;or could get a list of 3 filterwords, and trim it if  qfl, having a new qfl2 w/the smarts
 ;well not trim but only use some of the words to start then incr if necc, so fflf done there:qfl2

;for much of this a person in the loop would probably save the most time
 ;review-via/have something that will show full-initial org line, the qry &numbered so it could be selected ;w/last choice to run a diff qry
 ;look@ km QA tools ..
;look into the sparql results to tables
 
;csv
(ql 'cl-csv)
(defun read-csv-sp-str (sl)
  (explode-space (first-lv (first (cl-csv:read-csv sl)))))
(defun read-csv-fn (fn)
  (map-lines fn #'read-csv-sp-str))
 ;then can: (mapcar #'len *) & (histo *)
;stat
(ql 'lhstats)
(defun histo (l &optional (nbins 10))
  (statistics::histovalues l :nbins nbins))

;look at distribution of #of words, and make better use of it; 
;also when other info, like type of industry available, try to use in the query(find relation/ship for that)
; or course if there is already a url for it, just use that as a global id to map to

