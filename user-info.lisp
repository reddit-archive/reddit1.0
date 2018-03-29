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
 
(defmacro userinfo (info sym &optional article)
  (case sym
    (id `(user-info-id ,info))
    (user `(user-info-obj ,info))
    (saved (if article
               `(gethash ,article (user-info-saved ,info))
               `(user-info-saved ,info)))
    (closed (if article
                `(gethash ,article (user-info-closed ,info))
                `(user-info-closed ,info)))
    (clicked (if article
                 `(gethash ,article (user-info-clicked ,info))
                 `(user-info-clicked ,info)))
    (liked (if article
               `(gethash ,article (user-info-liked ,info))
               `(user-info-liked ,info)))
    ;;article == name
    (alias (if article
               `(gethash ,article (user-info-alias ,info))
               `(user-info-alias ,info)))))

(defmacro user-saved (info articleid)
  `(userinfo ,info saved ,articleid))

(defmacro user-clicked (info articleid)
  `(userinfo ,info clicked ,articleid))

(defmacro user-closed (info articleid)
  `(userinfo ,info closed ,articleid))

(defmacro user-liked (info articleid)
  `(userinfo ,info liked ,articleid))

(defmacro user-alias (info name)
  `(userinfo ,info alias ,name))

(defclass user-info ()
    ((id :initarg :id :initform (error "must specify an id"))
     (user :accessor user-obj)
     (options :accessor user-options)
     (saved :reader user-info-saved :initform (make-hash-table))
     (closed :reader user-info-closed :initform (make-hash-table))
     (clicked :reader user-info-clicked :initform (make-hash-table))
     (liked :reader user-info-liked :initform (make-hash-table))
     (alias :reader user-info-alias :initform (make-hash-table :test 'equal))))

(defun make-user-info (id)
  (when-bind (userobj (get-user id))
    (let ((info (make-instance 'user-info :id id)))
      (with-slots (user options) info
        (setf user userobj
              options (get-user-options id))
        (loop for articleid in
             (select [article] :from [saved_sites] :where [= [userid] id] :flatp t) do
             (setf (user-saved info articleid) t))
        (loop for articleid in
             (select [article] :from [clicks] :where [= [userid] id] :flatp t) do
             (setf (user-clicked info articleid) t))
        (loop for articleid in
             (select [article] :from [closed_sites] :where [= [userid] id] :flatp t) do
             (setf (user-closed info articleid) t))
        (loop for (articleid liked) in
             (select [article] [liked] :from [like_site] :where [= [userid] id] :flatp t) do
             (setf (user-liked info articleid) (if (string= liked "t") :like :dislike)))
        (loop for (name val) in
           (select [name] [val] :from [alias] :where [= [userid] id] :flatp t) do
             (setf (user-alias info name) val)))
      info)))

;;------------------------- user store ----------------------------
(defvar *user-info* (make-hash-table))

(defun load-info (id)
  (when-bind (info (make-user-info id))
    (log-message* "LOAD INFO: ~a" id)
    (setf (gethash id *user-info*) info)))

(defun get-info (id)
  (or
   (gethash id *user-info*)
   (load-info id)))

(defun remove-info (id)
  (remhash id *user-info*))
