;code that came w/KM components lib
;defun load-lib (&optional (comp-path "./components/"))
(defun load_lib (&optional (comp-path "./components/"))
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
