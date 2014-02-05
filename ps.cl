;http://common-lisp.net/project/parenscript/tutorial.html
;w/alterations to try to get it to work by bobak@balisp.org
(ql '(:hunchentoot :cl-who :parenscript :cl-fad))

(defpackage "PS-TUTORIAL"
    (:use "COMMON-LISP" "HUNCHENTOOT" "CL-WHO" "PARENSCRIPT" "CL-FAD"))

(in-package "PS-TUTORIAL")

(setf *js-string-delimiter* #\")

(start (make-instance 'acceptor :port 8080)) 

(define-easy-handler (tutorial1 :uri "/tutorial1") ()
  (with-html-output-to-string (s)
    (:html
     (:head (:title "Parenscript tutorial: 1st example"))
     (:body (:h2 "Parenscript tutorial: 1st example")
            "Please click the link below." :br
            (:a :href "#" :onclick (ps (alert "Hello World"))
                "Hello World"))))) 

(define-easy-handler (tutorial2 :uri "/tutorial2") ()
  (with-html-output-to-string (s)
    (:html
     (:head
      (:title "Parenscript tutorial: 2nd example")
      (:script :type "text/javascript"
               (str (ps
                      (defun greeting-callback ()
                        (alert "Hello World"))))))
     (:body
      (:h2 "Parenscript tutorial: 2nd example")
      (:a :href "#" :onclick (ps (greeting-callback))
          "Hello World"))))) 

(define-easy-handler (tutorial2-javascript :uri "/tutorial2.js") ()
  (setf (content-type*) "text/javascript")
  (ps
    (defun greeting-callback ()
      (alert "Hello World")))) 


(defvar *slideshows* (make-hash-table :test 'equalp))

(defun add-slideshow (slideshow-name image-folder)
  ;; we don't want to create duplicate dispatchers
  (unless (gethash slideshow-name *slideshows*)
    (push (create-folder-dispatcher-and-handler
           (format nil "/slideshow-images/~a/" slideshow-name)
           image-folder)
          *dispatch-table*))
  ;; but we do want to be able to refresh the cached list of image files
  (setf (gethash slideshow-name *slideshows*)
        (mapcar (lambda (pathname)
                  (url-encode (format nil "~a.~a"
                                      (pathname-name pathname)
                                      (pathname-type pathname))))
                (list-directory image-folder)))) 

(add-slideshow "pics" "/Users/bobak/Pictures/")

(defmacro+ps slideshow-image-uri (slideshow-name image-file)
;defmacro/ps slideshow-image-uri (slideshow-name image-file)
  `(concatenate 'string "/slideshow-images/" ,slideshow-name "/" ,image-file)) 

(defun slideshow-handler ()
  (cl-ppcre:register-groups-bind (slideshow-name) ("/slideshows/(.*)" (script-name*))
    (let* ((images (gethash slideshow-name *slideshows*))
           (current-image-index (or (position (get-parameter "image") images :test #'equalp)
                                    0))
           (previous-image-index (max 0 (1- current-image-index)))
           (next-image-index (min (1- (length images)) (1+ current-image-index))))
      (with-html-output-to-string (s)
        (:html
         (:head
          (:title "Parenscript slideshow")
          (:script :type "text/javascript"
                   (str (ps* `(progn
                                (var *slideshow-name* ,slideshow-name)
                                (var *images* (array ,@images))
                                (var *current-image-index* ,current-image-index)))))
          (:script :type "text/javascript" :src "/slideshow.js"))
         (:body
          (:div :id "slideshow-container"
                :style "width:100%;text-align:center"
                (:img :id "slideshow-img-object"
                      :src (slideshow-image-uri slideshow-name
                                                (elt images current-image-index)))
                :br
                (:a :href (format nil "/slideshows/~a?image=~a"
                                  slideshow-name (elt images previous-image-index))
                    :onclick (ps (previous-image) (return false))
                    "Previous")
                " "
                (:a :href (format nil "/slideshows/~a?image=~a"
                                  slideshow-name (elt images next-image-index))
                    :onclick (ps (next-image) (return false))
                    "Next"))))))))
     
(push (create-prefix-dispatcher "/slideshows/" 'slideshow-handler) 
      *dispatch-table*) 

(define-easy-handler (js-slideshow :uri "/slideshow.js") ()
  (setf (content-type*) "text/javascript")
  (ps
    (define-symbol-macro fragment-identifier (@ window location hash))
    
    (defun show-image-number (image-index)
      (let ((image-name (aref *images* (setf *current-image-index* image-index))))
        (setf (chain document (get-element-by-id "slideshow-img-object") src)
              (slideshow-image-uri *slideshow-name* image-name)
              fragment-identifier
              image-name)))
    
    (defun previous-image ()
      (when (> *current-image-index* 0)
        (show-image-number (1- *current-image-index*))))
      
    (defun next-image ()
      (when (< *current-image-index* (1- (getprop *images* 'length)))
        (show-image-number (1+ *current-image-index*))))

    ;; this gives bookmarkability using fragment identifiers
    (setf (getprop window 'onload)
          (lambda ()
            (when fragment-identifier
              (let ((image-name (chain fragment-identifier (slice 1))))
                (dotimes (i (length *images*))
                  (when (string= image-name (aref *images* i))
                    (show-image-number i))))))))) 
;Need to fix:
; in: DEFUN SLIDESHOW-HANDLER
;     (PARENSCRIPT:PS
;       (PS-TUTORIAL::PREVIOUS-IMAGE)
;       (RETURN PARENSCRIPT:FALSE))
; 
; caught WARNING:
;   Returning from unknown block nilBlock
; 
; caught ERROR:
;   during macroexpansion of
;   (PS
;     (PREVIOUS-IMAGE)
;     (RETURN FALSE)).
;   Use *BREAK-ON-SIGNALS* to intercept.
;   
;    The variable PARENSCRIPT::SUPPRESS-VALUES? is unbound.

;     (PARENSCRIPT:PS
;       (PS-TUTORIAL::NEXT-IMAGE)
;       (RETURN PARENSCRIPT:FALSE))
; 
; caught WARNING:
;   Returning from unknown block nilBlock
; 
; caught ERROR:
;   during macroexpansion of
;   (PS
;     (NEXT-IMAGE)
;     (RETURN FALSE)).
;   Use *BREAK-ON-SIGNALS* to intercept.
;   
;    The variable PARENSCRIPT::SUPPRESS-VALUES? is unbound.
; 
; compilation unit finished
;   caught 2 ERROR conditions
;   caught 2 WARNING conditions 
