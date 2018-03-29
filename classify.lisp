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

(defparameter *base-dir* #p"/home/reddit/data/")
(defparameter *user-dir* #p"/home/reddit/data/users/")
(defparameter *site-dir* #p"/home/reddit/data/sites/")
(defparameter *lynx* #p"/usr/local/bin/lynx")
(defparameter *classify* #p"/home/reddit/reddit/crm/classify.crm")
(defparameter *learn* #p"/home/reddit/reddit/crm/learn.crm")

(defun user-good-path (id)
  (merge-pathnames (make-pathname :name (format nil "good~a" id)
                                  :type "css")
                   *user-dir*))

(defun user-bad-path (id)
  (merge-pathnames (make-pathname :name (format nil "bad~a" id)
                                  :type "css") 
                   *user-dir*))

(defun site-path (id)
  (merge-pathnames (make-pathname :name (format nil "site~a" id))
                   *site-dir*))

(defun download-site (id url)
  (let ((sp (site-path id)))
    (ext:run-program *lynx* (list "-dump" url)
                     :output sp
                     :if-output-exists :supersede)))

(defun learn-site (userid siteid type)
  (let ((ufile (namestring (if (eql type :good) (user-good-path userid) (user-bad-path userid))))
        (sfile  (site-path siteid)))
    (and (probe-file sfile)
         (ext:run-program *learn* (list ufile)
                          :input sfile))))

(defun classify-site (userid siteid)
  (let ((gfile (namestring (user-good-path userid)))
        (bfile (namestring (user-bad-path userid)))
        (sfile (site-path siteid)))
    (and (probe-file gfile)
         (probe-file bfile)
         (probe-file sfile)
         (elt '(:good :bad :unknown)
          (ext:process-exit-code
                (ext:run-program *classify* (list gfile bfile)
                                 :input sfile))))))
