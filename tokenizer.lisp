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

(defparameter *body* (create-scanner "(?s)(?i)<body [^>]*>(.*)</body"))
(defparameter *tag* (create-scanner "(?s)<[^>]+?>"))

(defun html-body (html)
  (register-groups-bind (body) (*body* html)
    body))


(defparameter *min-length* 2)
(defparameter *max-length* 25)

(defun tokens-html-stream (str)
    (let ((tokens) (token))
      (loop for c = (read-char str nil)
         while c do
         (if (wordchar c)
             (progn
               (push c token))
             (progn
               (when (and (>= (length token) *min-length*)
                          (<= (length token) *max-length*))
                 (push (maketok (reverse token)) tokens))
               (setf token nil)
               (cond
                 ((char= c #\<) (readtag str))
                 ((char= c #\&) (skipescape str))))))
      tokens))
            

(defun readtag (str)
  ;;eventually read some tags
  (skiptag str))

(defun skiptag (str)
  (ignore-until str #\>))

(defun skipescape (str)
  (ignore-until str #\;))

(defun ignore-until (str e)
  (do ((c (read-char str nil nil) (read-char str nil nil)))
      ((or (not c) (eql c e)))))
          
(defun wordchar (c) (or (alpha-char-p c) (member c '(#\- #\' #\$ #\!))))

(defun whitechar (c) (member c '(#\Return #\Newline #\Space #\Tab)))

(defun maketok (chars)
  (intern (string-downcase (list->string  chars)) :keyword))

(defun list->string (lst)
  (map 'string #'(lambda (x) x) lst))

(defun tokens-url (url)
  (with-open-stream (in (website-stream url))
    (remove-duplicates (tokens-html-stream in))))

