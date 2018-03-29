;;;; Copyright 2018 Reddit, Inc.
;;;; 
;;;; Permission is hereby granted, free of charge, to any person obtaining a copy of
;;;; this software and associated documentation files (the "Software"), to deal in
;;;; the Software without restriction, including without limitation the rights to
;;;; use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
;;;; of the Software, and to permit persons to whom the Software is furnished to do
;;;; so, subject to the following conditions:
;;;; 
;;;; The above copyright notice and this permission notice shall be included in all
;;;; copies or substantial portions of the Software.
;;;; 
;;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;;;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;;;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;;;; SOFTWARE.

(in-package :reddit)

(defparameter *STORED* "STORED")
(defparameter *NOTSTORED* "NOT_STORED")
(defparameter *END* "END")
(defparameter *DELETED* "DELETED")
(defparameter *NOTFOUND* "NOT_FOUND")
(defparameter *OK* "OK")
(defparameter *ERROR* "ERROR")
(defparameter *CLIENTERROR* "CLIENT_ERROR")
(defparameter *SERVERERROR* "SERVER_ERROR")
(defparameter *VALUE* "VALUE")

(defparameter *cache* (make-hash-table :test 'equal))

(defmacro cached ((key &optional (exp 0)) &body body) 
  (let ((k (gensym)))
    `(let ((,k ,key))
        (or (mc-get ,k)
            (let ((val (progn ,@body)))
              (mc-set ,k val ,exp)
              val)))))

;;TODO more servers
(defun get-stream ()
  (ignore-errors
    (sys:make-fd-stream (ext:connect-to-inet-socket "127.0.0.1" 11211)
                        :input t :output t 
                        :buffering :none
                        :auto-close t)))

(defun mc-write-str (str stream)
  (write-string str stream)
  (write-char #\Return stream)
  (write-char #\Newline stream))

(defun mc-read-str (stream &optional len)
  ;(force-output stream)
  (if len
      ;;read len bytes in as few reads as possible
      (let ((val (read stream)))
        (read-char stream) (read-char stream)
        val)
      ;;everything else is read as one line
      (let ((str (read-line stream)))
        (subseq str 0 (1- (length str))))))
  
;;TODO locking!
(defun mc-store (cmd key val &optional (exp 0))
  (with-open-stream (s (get-stream))
    (when s
      (let ((cmd-str (case cmd
                   (:add "add")
                   (:replace "replace")
                   (t "set")))
          (val-str (with-output-to-string (s) (prin1 val s))))
      (mc-write-str (format nil "~a ~a ~a ~a ~a" cmd-str key 0 exp (length val-str)) s)
      (mc-write-str val-str s)
      (let ((response (mc-read-str s)))
        (cond
          ((string= *STORED* response) :STORED)
          ((string= *NOTSTORED* response) :NOTSTORED)
          (t response)))))))

(defun mc-set (key val &optional (exp 0))
  (mc-store :set key val exp))

(defun mc-add (key val &optional (exp 0))
  (mc-store :add key val exp))
                
(defun mc-replace (key val &optional (exp 0))
  (mc-store :replace key val exp))

(defun parse-value (value-str)
  (let* ((s1 (position #\space value-str :start 6))
         (s2 (position #\space value-str :start (1+ s1)))
         (key (subseq value-str 6 s1))
         (flags (parse-integer (subseq value-str (1+ s1) s2)))
         (len (parse-integer (subseq value-str (1+ s2)))))
    (list key flags len)))

(defun mc-read-val (stream)
  (let ((response (mc-read-str stream)))
        (when (string= response "VALUE" :end1 (min (length response) 5))
          (destructuring-bind (key flags len) (parse-value response)
            (values
             (mc-read-str stream len)
             key flags)))))

(defun mc-get (key)
  (with-open-stream (stream (get-stream))
    (when stream
      (mc-write-str (format nil "get ~a" key) stream)
      (let ((val (mc-read-val stream)))
        (when val
          ;;read END
          (mc-read-str stream)
          val)))))

(defun mc-delete (key &optional (time 0))
  (with-open-stream (stream (get-stream))
    (when stream
    (mc-write-str (format nil "delete ~a ~a" key time) stream)
    (let ((response (mc-read-str stream)))
      (cond
        ((string= response *DELETED*) :DELETED)
        ((string= response *NOTFOUND*) :NOTFOUND)
        (t response))))))
  
