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

(in-package #:reddit)

(defparameter *title* (create-scanner "(?s)(?i)<title>(.+?)</title>"))
(defparameter *toplevel* (create-scanner "https?://(?:www.)?([^/]*)"))

(defun website-stream (url) 
  (third (http-get url)))

(defun website-string (url)
  (let ((in (third (http-get url))))
    (with-output-to-string (s)
      (when in
        (loop for line = (read-line in nil)
           while line do (format s "~a~%" line))
        (close in))
      s)))

(defun website-title (url)
  (log-message* "downloading title for ~a" url)
  (ignore-errors
    (register-groups-bind (title) (*title* (website-string url))
      (remove #\Newline (remove #\Return title)))))

(defun tl-domain (url)
  (ignore-errors
    (register-groups-bind (tl-dom) (*toplevel* url)
      tl-dom)))

(defun replace-alist (alist newlist)
  (remove nil
          (append newlist
                  (mapcar #'(lambda (param)
                              (unless (assoc (car param) newlist)
                                param))
                          alist))))

(defun create-url (base params)
  (with-output-to-string (s)
    (format s "~a?" base)
    (loop for param in params
         for x = 0 then (1+ x) do
         (when (> x 0) (format s "&"))
         (format s "~a=~a" (car param) (cdr param)))))

(defun minutes (seconds)
  (floor (/ seconds 60)))

(defun hours (seconds)
  (floor (/ seconds 3600)))

(defun days (seconds)
  (floor (/ seconds 86400)))

(defun age-str (date)
  (multiple-value-bind (usec second minute hour day month year) (decode-time date)
    (let* ((utime (encode-universal-time second minute hour day month year))
           (seconds (- (get-universal-time) utime)))
      (cond ((< seconds 7199) (format nil "~a minute~:p" (minutes seconds)))
            ((< seconds 86400) (format nil "~a hour~:p" (hours seconds)))
            (t (format nil "~a day~:p" (days seconds)))))))
                         
(defun sanitize (in type &optional valid-inputs)
  (when in
    (case type
      (int (parse-integer in :junk-allowed t))
      (string (if valid-inputs
                  (when (member in valid-inputs :test #'string=) in)
                  in))
      (sym (let ((ustr (intern (string-upcase in) :keyword)))
             (if valid-inputs
                 (when (member ustr valid-inputs) ustr)
                 ustr))))))

(defun add-rlist (val lst size)
  (if (member val lst)
      lst
      (cons val (subseq lst 0 (1- size)))))

;;user url
(defparameter *user-url* "/user/([^/]+)/?([^/]*)")

(defun decode-user-url (url)
  (register-groups-bind (name fn) (*user-url* url)
    (values name fn)))

;;cookies
(defun 2weeks () 
  (+ (get-universal-time) 1209600))

(defun -2weeks ()
  (- (get-universal-time) 1209600))

(defun 5minutes ()
  (+ (get-universal-time) 300))

(defun set-cookie-list (name lst)
  (set-cookie name :value (format nil "~{~a~^:~}" lst)
              :expires (2weeks)))

(defun add-to-cookie-list (val name lst)
  (let ((lst (if (member val lst)
                 lst
                 (cons val lst))))
    (set-cookie-list name lst)
    lst))

(defun get-cookie-list (name)
  (mapcar #'(lambda (val) (sanitize val 'int))
              (split ":" (cookie-in name))))

;;ny times link
(defparameter *userland* "rssuserland")
(defparameter *goodnytimes* "(?s)<a href=\\\"(http://www.nytimes.com[^\\\"]+?userland[^\\\"]+?)\\\"")
(defparameter *nytimes* "http://www.nytimes.com")

(defun good-nytimes-p (url)
  (scan *userland* url))

(defun nytimes-link-p (url)
  (not (mismatch *nytimes* url :end2 (length *nytimes*))))

(defun nytimes-genlink-website (url)
  (website-string (conc "http://nytimes.blogspace.com/genlink?q=" url)))

(defun good-nytimes (url)
    (ignore-errors
      (register-groups-bind (goodurl) (*goodnytimes* (nytimes-genlink-website url))
        goodurl)))

(defun nytimes-safe-url (url)
  (if (and (nytimes-link-p url)
           (not (good-nytimes-p url)))
      (or (good-nytimes url)
          url)
      url))
      

;;important part of urls
(defparameter *baseurl* (create-scanner "(?:https?://)?(?:www.)?([^#]*)"))

(defun base-url (url)
  "Removes the http://www and any anchors from a url."
  (let ((burl (ignore-errors
                (register-groups-bind (burl) (*baseurl* url)
                  (or burl
                      url)))))
    (when burl
      (if (char= #\/ (char burl (1- (length burl))))
          (subseq burl 0 (1- (length burl)))
          burl))))

(defun add-http (url)
  "Add http:// to a url if http:// or https:// isn't already present."
  (or (and (mismatch "http://" url :end2 7)
           (mismatch "https://" url :end2 8)
           (concatenate 'string "http://" url))
      url))

(defun makestr (&rest args)
  (format nil "~{~a~}" args))

(defun key-str (&rest args)
  "Returns a string representation of the arguements with no spaces."
  (substitute #\_ #\space (format nil "~{~a~^-~}" args)))

(defun esc-quote (str)
  "Returns a string with ' escaped."
  (escape-string str :test #'(lambda (c) (char= c #\'))))

(defun shorten-str (str len)
  (subseq str 0 (min len (length str))))

(defmacro when-bind ((var expr) &body body)
  `(let ((,var ,expr))
     (when ,var
       ,@body)))

(defmacro when-bind* (binds &body body)
  (if (null binds)
      `(progn ,@body)
      `(let (,(car binds))
         (if ,(caar binds)
             (when-bind* ,(cdr binds) ,@body)))))
