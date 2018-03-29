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

(defparameter *mail-prog* "/usr/bin/mail")

(defparameter *user* "ZG1haWw=")
(defparameter *pass* "Ymxhcmc=")
(defparameter *mail-server* "216.55.162.13")

(defun send-reddit-email (to from subject message)
  (mp:make-process
   #'(lambda ()
         (ignore-errors
             (send-email *mail-server* from to subject message :username *user* :password *pass*)))))

(defun info-email-body (user password)
  (with-output-to-string (s)
    (format s "Your login information for http://reddit.com is:~%~%")
    (format s "~4tUsername: ~a~%" user)
    (format s "~4tPassword: ~a~%~%" password)
    (format s "Thank you for using reddit.com!")))

(defun send-login-info (to user pass)
  (send-reddit-email to "reddit@reddit.com" "reddit.com login information"
                     (info-email-body user pass)))

(defun recommend-email-body (from title link &optional personal)
  (with-output-to-string (s)
    (format s "This email was sent to you by: ~a~%~%" from)
    (if personal
        (format s "~a~%~%" personal)
        (format s "A user at reddit.com thought you would find this link interesting:~%~%"))
    (format s "~a~%" title)
    (format s "~a~%~%" link)
    (format s "Check out http://reddit.com to see what's new online today!~%~%")
    (format s "If you have any questions regarding this email direct them to feedback@reddit.com")))

(defun send-recommendation (userid articleid ip addresses from personal)
  (let* ((tl (site-tl articleid))
         (title (first tl))
         (url (second tl))
         (sub (format nil "reddit.com: ~a" title))
         (body (recommend-email-body from title url personal)))
    (dolist (to (cond ((listp addresses)
                       addresses)
                      ((atom addresses) (list addresses))))
      (when (valid-email userid ip to)
        (send-reddit-email to from sub body)
        (email-sent userid articleid ip to)))))

