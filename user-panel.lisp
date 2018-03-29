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

(defun saved-panel (name)
  (let ((sites (saved-sites (valid-user-p name))))
    (pbox (fmt "~a's saved sites" name)
      (:table
       (loop for (id title url) in sites do
            (htm (:tr (:td (site-link id title url)))))))))

(defun date-string (sql-date)
  (destructuring-bind (year month day) (split "-" sql-date)
    (format nil "~a ~a, ~a" (month-name (parse-integer month)) day year)))

(defmacro left-el (l)
  `(with-html-output(*standard-output*)
     (:td :nowrap t :class "profline" ,l)))

(defmacro prof-row (l r)
  `(with-html-output (*standard-output*)
     (:tr (left-el ,l)
          (:td :style "padding-bottom: 5px" :nowrap t (esc ,r)))))

(defun me (name)
  (and (logged-in-p)
       (string= name (user-name (userobj)))))

(defun base-info-panel (name)
  (destructuring-bind ((karma date) (total up down)) (list (basic-info name) (user-stats name))
    (setf karma (or karma "0"))
    (with-html-output (*standard-output*)
      (:div :class "meat"
      (user-heading name :basic)
      (:table
       (prof-row "karma:" karma)
                    (prof-row "member since:" (date-string date))
                    (when (me name)
                      (prof-row "email:" (or (user-email name) "not entered - see the update panel below"))))
                    (hbar  "stats")
                    (:table
                    (prof-row "total submissions:" total)
                    (prof-row "sites promoted:" up)
                    (prof-row "sites demoted:" down))
                    (when (me name)
                      (with-accessors ((limit options-numsites)
                                       (visible options-visible)
                                       (promoted options-promoted)
                                       (demoted options-demoted)
                                       (frame options-frame)) (options)
                      (htm
                       (hbar "options")
                       (:table :id "options" :style "border-collapse: collapse;"
                               (:tr (left-el "open reddit links in a frame:")
                                    (:td (:input :name "frame" :type "checkbox" :checked frame)))
                               (:tr (left-el "make my profile visible to other users:")
                                    (:td (:input :name "vis" :type "checkbox" :checked visible)))
                               (:tr (left-el "show me sites I've promoted:")
                                    (:td (:input :name "pro" :type "checkbox" :checked promoted)))
                               (:tr (left-el "show me sites I've demoted:")
                                    (:td (:input :name "dem" :type "checkbox" :checked demoted)))
                               (:tr (left-el "number of sites to display at once:")
                                    (:td (:select :name "limit" 
                                                  (:option :selected (= limit 10) :value "10" "10")
                                                  (:option :selected (= limit 25) :value "25" "25")
                                                  (:option :selected (= limit 50) :value "50" "50")))
                                    (:td (:input :type "submit" :class "btn" :value "save" 
                                                               :onclick "options()")))
                               (:tr (:td :class "error" :id "optstat")))
                       (hbar "update")
                       (:table :style "border-collapse: collapse;"
                               (:tr (left-el "email:")
                                    (:td (:input :id "upemail" :type "text")) 
                                    (:td (:input :type "submit" :class "btn" :value "update" :onclick "uemail()")))
                               (:tr (:td) (:td :colspan "2" :id "upemailerr" :class "error"))
                               (:tr (left-el "current password:")
                                    (:td (:input :id "upcur" :type "password")))
                               (:tr (left-el "new password:")
                                    (:td (:input :id "upnew" :type "password")))
                               (:tr (left-el "verify password:")
                                    (:td (:input :id "upver" :type "password"))
                                    (:td  (:input :type "submit" :class "btn" :value "update" :onclick "upass()")))
                               (:tr (:td) (:td :colspan "2" :id "uppasserr" :class "error"))))))))))

(defun user-menu (username)
  (let ((prefix (conc "/user/" username)))
    (mapcar #'(lambda (name)
                (list (intern (string-upcase name) :keyword) name (conc prefix "/" name)))
            (cond
              ((me username) '("basic" "submitted" "hidden" "promoted" "demoted"))
              ((profile-visible (valid-user-p username)) '("basic" "saved" "submitted" "hidden" "promoted" "demoted"))
              (t '("basic" "submitted"))))))

(defun user-heading (user &optional selected)
  (with-html-output (*standard-output*)
    (let ((menu (user-menu user)))
      (htm
      (:div :id "usermenu" (:span :class "username" (esc user))
            (loop for (sym title url) in menu do
                 (htm
                  (:a :class (when (eql sym selected) "sel-user")
                      :href url
                      (esc title)))))))))
         
(defun right-panel-user (user page)
  (with-html-output (*standard-output*)
    (unless (logged-in-p) (login-panel))))

(defun profile-site-table (profid display)
  (with-parameters ((offset "offset"))
    (setf offset (or (sanitize offset 'int) 0))
    (multiple-value-bind (articles nextoff)
        (get-sites-profile (uid) profid (options-numsites (options)) offset display)
      (site-table articles (options-numsites (options)) offset
                  nextoff (and (eql display :saved)
                               (logged-in-p)
                               (= (uid) profid))
                  (eql display :hidden)))))


(defun profile-page (name display)
  (with-html-output (*standard-output*)
    (:div :class "meat"
    (if (or (profile-visible (valid-user-p name)) (eql display :submitted) (me name))
        (htm
         (user-heading name display)
         (profile-site-table (valid-user-p name) display))
        (htm (:span :class "error" "this page of the user's profile is not public"))))))

(defun user-panel (user page &optional me)
    (case page
      (:submitted (profile-page user :submitted))
      (:promoted  (profile-page user :promoted))
      (:demoted  (profile-page user :demoted))
      (:saved  (profile-page user :saved))
      (:hidden  (profile-page user :hidden))
      (t (base-info-panel user))))

(defun page-user ()
  (multiple-value-bind (user page) (decode-user-url (script-name))
    (let ((user (valid-user-p user :return-sn t)))
      (if user
        (let ((page (sanitize page 'sym '(:basic :saved :promoted :submitted :hidden :demoted))))
          (reddit-page (:menu (top-menu (browse-menu))
                        :right-panel (right-panel-user user page))
            (user-panel user page (and (logged-in-p) (string= (user-name (userobj)) user)))))
        (reddit-page ()
          (:span :class "error" "That user does not exist"))))))
