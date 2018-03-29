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

(defparameter *secret* "blargo")

(defun iso-time ()
  (let ((itime (format-time nil (get-time) :format :iso)))
    (subseq itime 0 (position #\, itime))))

(defun hashstr (str)
  (byte-array-to-hex-string (digest-sequence :sha256 (ascii-string-to-byte-array str))))

(defun cookie-str (sn pass)
  (let ((time (iso-time)))
    (makestr sn "," time ","
             (hashstr (makestr time sn pass *secret*)))))

(defun valid-cookie (str)
  "returns the userid for cookie if valid, otherwise nil"
  (when (= (count #\, str :test #'char=) 2)
    (when-bind* ((sn (subseq str 0 (position #\, str :test #'char=)))
                 (time (subseq str (+ 1 (length sn)) (position #\, str :from-end t :test #'char=)))
                 (hash (subseq str (+ (length sn) (length time) 2)))
                 (pass (user-pass sn)))
      (when (string= hash (hashstr (makestr time sn pass *secret*)))
        (user-id (get-user sn))))))


  
             
  

