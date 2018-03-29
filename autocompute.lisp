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

(defun get-processes (name)
  (remove-if-not #'(lambda (x) (string= name (mp:process-name x)))
                 (mp:all-processes)))

(defun destroy-processes (name)
  (dolist (p (get-processes name))
    (mp:destroy-process p)))

(defclass ac ()
  ((name
    :initarg :name
    :initform (error "must specify a name")
    :reader ac-name)
   (process
    :reader ac-process)
   (val
    :initform nil
    :reader ac-val)
   (period
    :initarg :period
    :initform (error "must specify a period")
    :accessor ac-period)
   (fn
    :initarg :fn
    :initform (error "must specify a function")
    :accessor ac-fn)
   (lock
    :initform (mp:make-lock)
    :accessor ac-lock)))

(defmethod initialize-instance :after ((auto ac)  &key)
  (destroy-processes (ac-name auto))
   (setf (slot-value auto 'process)
         (mp:make-process
          #'(lambda ()
              (loop
                 (mp:with-lock-held ((ac-lock auto))
                   (setf (slot-value auto 'val)
                         (funcall (ac-fn auto))))
                 (sleep (ac-period auto))))
  :name (ac-name auto))))

(defmethod ac-update ((auto ac))
  (mp:with-lock-held ((ac-lock auto))
    (setf (slot-value auto 'val)
          (funcall (ac-fn auto)))))
             
