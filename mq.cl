;; Simple client server
(ql 'zeromq)
;; from http://www.cliki.net/cl-zmq but w/edits to try to get it to load bobak@balisp.org
;;need: http://web.archive.org/web/*/http://www.cs.rice.edu/~froydnj/lisp/packer.tar.gz
(al 'packer)
(ql 'iterate) ;still some problems here
(use-package :iterate)
;could write a defpackage

(defun server ()
  "Bind to socket and wait to receive a message.  After receipt,
  return the message \"OK\"."
  (zmq:with-context (ctx 1)
    (zmq:with-socket (socket ctx zmq:rep)
      (zmq:bind socket "tcp://lo:5555")
      (loop
       (let ((query (make-instance 'zmq:msg)))
         (zmq:recv socket query)
         (format t "Recieved query: '~A'~%"
                 (zmq:msg-data-as-string query) ))
       (zmq:send socket (make-instance 'zmq:msg :data "OK")) ))))

(defun client ()
  "Connects to a socket and passes a message and waits to receive the
  \"OK\" from the server."
  (zmq:with-context (ctx 1)
    (zmq:with-socket (socket ctx zmq:req)
      (zmq:connect socket "tcp://localhost:5555")
      (zmq:send socket (make-instance 'zmq:msg
                                      :data "SELECT * FROM mytable" ))
      (let ((result (make-instance 'zmq:msg)))
        (zmq:recv socket result)
        (format t "Recieved string: '~A'~%"
                (zmq:msg-data-as-string result) )))))

;; Publish/subscribe example (uses _(iterate) and _(packer))

(defun publisher ()
  (zmq:with-context (ctx 1)
    (zmq:with-socket (socket ctx zmq:pub)
      (zmq:bind socket "tcp://lo:5555")
      (iter (for msg-id from 0)
      ;iterate:iter (iterate:for msg-id iterate:from 0)
            (zmq:send
             socket
             (make-instance
              'zmq:msg
              :data (coerce (packer:pack ">L" msg-id)
                            '(vector (unsigned-byte 8)) )))))))

(defun subscriber ()
  (zmq:with-context (ctx 1)
    (zmq:with-socket (socket ctx zmq:sub)
      (zmq:setsockopt socket zmq:subscribe "")
      (zmq:connect socket "tcp://localhost:5555")
      (loop
        (let ((query (make-instance 'zmq:msg)))
          (zmq:recv socket query)
          (let ((msg-id (first (packer:unpack
                                ">L" (zmq:msg-data-as-array query) ))))
            (when (= 0 (mod msg-id 10000))
              (print msg-id) ))))))) 
