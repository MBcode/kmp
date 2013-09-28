;code that came w/KM components lib
;refs:
;http://www.cs.utexas.edu/users/mfkb/RKF/clib.html 
;http://www.cs.utexas.edu/users/mfkb/RKF/trunktree/releases.cgi 
;http://www.cs.utexas.edu/users/mfkb/RKF/tree/ 
;http://web.archive.org/web/20110514122814/http://www.cs.utexas.edu/users/kbarker/papers/kcap01-content.pdf
(defun load_lib (&optional (comp-path "components/core/"))
  (let ((kmf (files-of-p comp-path #'(lambda (s) (suffixp ".km" s)))))
    (mapcar- #'load-kb kmf))) ;could be one line

;Had to rewrite what I found above w/the short fnc above 
;defun load-lib (&optional (comp-path "./components/"))
(defun load_lib- (&optional (comp-path "./components/"))
  (let (result)
    (dolist (component (traverse-directory comp-path) result)
      (cond 
        ((equal (pathname-type component) "km") 
         (load-kb component)
         (setf result (cons (pathname-name component) result)))
        ((equal (pathname-type component) "lisp")
         (load component))))))

;defun traverse-directory (root-dir-path)
(defun traverse-directory (root-dir-path &optional (depth 0) (max-depth 10))
 (when (< depth max-depth)
  (let ((dir-list (directory root-dir-path)) sub-dir-path result)
    (dolist (element dir-list result)
      (if (directory (setf sub-dir-path (concatenate 'string root-dir-path (pathname-name element) "/"
)))
         ;(setf result (append (traverse-directory sub-dir-path) result))
          (setf result (append (traverse-directory sub-dir-path (1+ depth)) result))
          (setf result (cons element result))))))
) 

(trace load_lib traverse-directory)
