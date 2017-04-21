;; Copyright Mark Watson 2001-2013. All Rights Reserved.  http://markwatson.com
;; License: AGPL version 3 (http://www.gnu.org/licenses/agpl-3.0.txt)
;; Alternative license: you may also use this software under the Apache 2 License.
;; This copyright notice should not be removed from this file and in files derived from this file.
;https://github.com/mark-watson/loving-common-lisp/blob/master/src/dbpedia-lookup.lisp
(ql:quickload :drakma)
(ql:quickload :babel)
(ql:quickload :s-xml)

;; utility from http://cl-cookbook.sourceforge.net/strings.html#manip:
(defun replace-all (string part replacement &key (test #'char=))
  "Returns a new string in which all the occurrences of the part 
is replaced with replacement."
  (with-output-to-string (out)
    (loop with part-length = (length part)
       for old-pos = 0 then (+ pos part-length)
       for pos = (search part string
                         :start2 old-pos
                         :test test)
       do (write-string string out
                        :start old-pos
                        :end (or pos (length string)))
       when pos do (write-string replacement out)
       while pos)))

(defstruct dbpedia-data uri label description)

(defun cadr-lv (l) (first-lv (rest-lv l)))

(defun dbpedia-lookup (search-string &optional (os t) (extra nil))
  (let* ((s-str (replace-all search-string " " "+"))
         (s-uri 
          (concatenate
           'string
           "http://lookup.dbpedia.org/api/search.asmx/KeywordSearch?QueryString="
           s-str))
         (response-body nil)
         (response-status nil)
         (response-headers nil)
         (xml nil)
         ret)
    (multiple-value-setq
        (response-body response-status response-headers)
      (drakma:http-request
       s-uri
       :method :get
       :accept "application/xml"))
    ;;(print (list ("status:" response-status "headers:" response-headers)))
    (setf xml
          (s-xml:parse-xml-string
           (babel:octets-to-string response-body)))
   (when (listp xml)
    (dolist (r (cdr xml))
      ;; assumption: data is returned in the order:
      ;;   1. label
      ;;   2. DBPedia URI for more information
      ;;   3. description
     (when (len-gt r 3) ;or nth-lv
      (let ((uri (cadr (nth 2 r)))
            (label (cadr (nth 1 r)))
           ;(description (string-trim '(#\Space #\NewLine #\Tab) (cadr (nth-lv 3 r))))
            (description (str_trim (cadr-lv (nth-lv 3 r))))
            )
        (if extra
        ;(format os "~%~a, ~a, ~a, ~a, ~a" search-string uri label description extra)
         (format os "~%~a, ~a, ~a, ~a, ~a" extra uri search-string label description )
         (format os "~%~a, ~a, ~a, ~a" search-string uri label description))
      (push
       (make-dbpedia-data
        :uri uri
        :label label
        :description description)
       ret)))))
    (reverse ret)))

;; (dbpedia-lookup "berlin")

(defun lookup-all (l)
  (with-open-file (os "lookup.tmp" :direction :output)
    (mapcar #'(lambda (x) (dbpedia-lookup x os)) l)))
;my/mb add
(defun dbl (s) (dbpedia-lookup (clean4echo s)))
