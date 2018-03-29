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

(defun user-options (id)
  "Returns a user's options"
       (mapcar #'(lambda (val sym) (cons sym val))
               (or (mapcar #'(lambda (arg)
                               (cond ((numberp arg) arg)
                                     ((string= "t" arg) t)
                                     (t nil)))
                           (and id
                                (car (select [visible] [promoted] [demoted] [numsites]
                                             :from [options]
                                             :where [= [userid] id]
                                             :flatp t))))
                   '(nil t nil 25))
               '(:visible :promoted :demoted :numsites)))
              

(defun sql-bool (bool)
  (if bool [true] [false]))

(defun set-user-options (id vis pro dem numsites)
  "Sets a user's options"
  (let ((options `((visible ,(sql-bool vis))
                   (promoted ,(sql-bool pro))
                   (demoted ,(sql-bool dem))
                   (numsites ,numsites))))
    (if (select [userid] :from [options] :where [= [userid] id])
        (update-records [options]
                        :av-pairs options
                        :where [= [userid] id])
        (insert-records :into [options]
                        :av-pairs (cons `(userid ,id) options)))))


(defun user-vis-p (userid)
  (string= "t" (car (select [visible] :from [options]
                            :where [= [userid] userid]
                            :flatp t))))

