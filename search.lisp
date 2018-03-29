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

(defun search-char (c)
  "T if c is an a letter, number, or '"
  (or (alphanumericp c)
      (member c '(#\' #\space #\")))) 

(defmacro rra (rex new str)
  "Make regex-replace-all lest awkward to type"
  `(regex-replace-all ,rex ,str ,new))
  
(defun to-search-str (str)
  "Formats the incoming str into a valid tsearch string"
  (rra "(?i) or | " "|"
       (rra "\\s+$" ""
            (rra "^\\s+" ""
                 (rra "(?i) and " "&"
                      (rra "\\s+" " "
                           (rra "\\\"" "'" 
                                (remove-if-not #'search-char str))))))))
                     
(defun search-sites (str &optional (limit 25) (offset 0))
  (when (> (length str) 0)
    (let ((q (format nil "to_tsquery('default', ~a)" (sql (to-search-str str)))))
      (select 'article-with-sn
              :where (sql-expression :string (format nil "idxfti @@ ~a" q))
              :order-by `((,(sql-expression :string (format nil "rank(idxfti, ~a)" q)) desc)
                          (,[articles_sn date] desc))
              :offset offset
              :limit limit
              :flatp t))))

                  
  





