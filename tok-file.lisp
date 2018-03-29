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

;;TODO - write a pathname macro such that it'll define these functions
;;with an &optional parameter for the pathname

;;TODO - lock resources that will be accessed by multiple threads -
;;user data and site data

(defparameter *base-dir* #p"/home/reddit/data/")
(defparameter *max-users* 2)
(defparameter *max-sites* 100)

(defvar *site-tokens* (make-hash-table))
(defvar *user-tokens* (make-hash-table))

(defun site-pathname (articleid)
  "Returns the pathname for a site datafile."
  (merge-pathnames (make-pathname :name (format nil "site~a" articleid))
                   *base-dir*))

(defun user-pathname (userid type)
  "Returns the pathname for userid's file. Type should be :good
  or :bad."
  (merge-pathnames (make-pathname :name (format nil "user~a~a" userid
                                                (case type
                                                  (:good "g")
                                                  (:bad "b")
                                                  (t (error "good or bad" )))))
                   *base-dir*))

(defun write-site-tokens (articleid tokens)
  "Writes the list of tokens to a file."
  (with-open-file (out (site-pathname articleid)
                              :direction :output
                              :if-exists :supersede
                              :if-does-not-exist :create)
    (when out
      (print tokens out))))

(defun read-site-tokens (articleid)
  "Reads the list of tokens from a file."
  (with-open-file (in (site-pathname articleid)
                      :direction :input
                      :if-does-not-exist nil)
    (when in
      (read in))))

(defun download-site (id url)
  (ignore-errors
    (write-site-tokens id (tokens-url url))))

(defun write-hash-table (path ht)
  "Writes ht to path with a newline between keys and values."
  (with-open-file (out path
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create)
    (when out
      (loop for key being the hash-keys in ht using (hash-value val) do
           (print key out)
           (print val out)))))

(defun read-hash-table (path)
  "Reads alternating keys and vals from path and returns a
  hashtable."
  (with-open-file (in path
                      :direction :input
                      :if-does-not-exist nil)
    (when in
      (let ((ht (make-hash-table)))
        (loop
           for key = (read in nil nil)
           for val = (read in nil nil)
           while (and key val) do
             (setf (gethash key ht) val))
        ht))))

(defun write-user-tokens (userid goodht badht)
  "Writes the user's good/bad tokens to their respective files."
  (write-hash-table (user-pathname userid :good) goodht)
  (write-hash-table (user-pathname userid :bad) badht))

(defun read-user-tokens (userid)
  "Reads a user's good/bad tokens from their respective files."
  (list
   (or (read-hash-table (user-pathname userid :good)) (make-hash-table))
   (or (read-hash-table (user-pathname userid :bad)) (make-hash-table))))

;;TODO functions to write for a specific user, or write all users
(defmacro max-size-hash (name maxsize read-fn &optional write-fn)
  "Defines a closure keeps a hashtable under a max size,
  populating it with read-fn when required."
  `(let ((max ,maxsize)
         (fn ,read-fn)
         (wfn ,write-fn)
         (viewed nil)
         (ht (make-hash-table)))
     (defun ,name (key)
       (multiple-value-bind (data exists) (or (gethash key ht) (funcall fn key) )
         (when data
             (setf viewed (cons key (remove key viewed)))
             (when (> (length viewed) max)
               (let ((l (car (last viewed))))
                 ;;write user data
                 (when wfn
                   (let ((udata (gethash l ht)))
                     (funcall wfn l (first udata) (second udata))))
                 (remhash l ht)
                 (setf viewed (remove l viewed :from-end t))))
             (unless exists
               (setf (gethash key ht) data))
             data)))))

(max-size-hash user-data 10 #'read-user-tokens #'write-user-tokens)
(max-size-hash site-data 100 #'read-site-tokens)

(defun flag-site (userid articleid goodbad)
  "Adds a site's tokens to a user's good/bad table."
  (let ((userht (case goodbad
                  (:good (first (user-data userid)))
                  (:bad (second (user-data userid)))
                  (t (error "good or bad"))))
        (sdata (site-data articleid)))
    (loop for token in sdata do
         (setf (gethash token userht) (1+ (gethash token userht 0))))))

(defun token-prob (token goodht badht)
  "The probability that a token is good. .01 == max bad, .99 ==
  max good."
  (let ((g (gethash token goodht 0))
        (b (gethash token badht 0))
        (gsize (max 1 (hash-table-count goodht)))
        (bsize (max 1 (hash-table-count badht))))
    (if (>= (+ g b) 5)
      (max .01
           (min .99 (float (/ (min 1 (/ g gsize))
                              (+ (min 1 (/ g gsize))
                                 (min 1 (/ b bsize)))))))
      .5)))

(defun most-interesting (tokens goodht badht)
  "Returns a list of the 15 most interesting (token . prob) pairs
  where interesting is the distance from .5 of prob."
  (let ((probs (sort
                (mapcar #'(lambda (token)
                            (cons token (token-prob token goodht badht)))
                        tokens)
                #'> :key #'(lambda (x) (abs (- .5 (cdr x)))))))
    (subseq probs 0 (min 15 (length probs)))))

(defun combine-probs (probs)
  "Returns the combination of probabilities in prob."
  (max .01
       (min .99
            (let ((prod (apply #'* probs)))
              (/ prod (+ prod (apply #'* (mapcar #'(lambda (x)
                                                     (- 1 x))
                                                 probs))))))))

(defun site-prob (userid articleid)
  "Returns the probability that userid thinks articleid is good."
  (let ((sdata (site-data articleid))
        (udata (user-data userid)))
    (combine-probs (mapcar #'(lambda (x) (cdr x))
                         (most-interesting sdata (first udata) (second udata))))))


