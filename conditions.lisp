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

(define-condition invalid-user ()
  ((username :initarg :username :reader username)
   (password :initarg :password :reader password)))

(define-condition article-exists ()
  ((id :initarg :id :reader id)
   (url :initarg :url :reader url)
   (title :initarg :title :reader title)))

(define-condition article-submitted ()
  ((id :initarg :id :reader id)
   (url :initarg :url :reader url)
   (title :initarg :title :reader title)))

(define-condition check-article-title ()
  ((title :initarg :title :reader title)))

(define-condition user-exists ()
  ((username :initarg :username :reader username)))

(define-condition incorrect-password ()
  nil)

(define-condition password-changed ()
  nil)


