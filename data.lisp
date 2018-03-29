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

(defparameter *max-emails* 100)
(defparameter *mod-window* (make-duration :day 2))
(defparameter *min-mod-karma* 0)
(defparameter *min-karma* 10)

(defvar *database-type* :postgresql)
(defvar *database-name* "reddit")
(defvar *database-user* "pgsql")
(defvar *database-server* "")
(defvar *database-password* "pgcwip42:")

(defparameter *conn-spec* `(,*database-server* ,*database-name* ,*database-user* ,*database-password*))

(defmacro with-web-db (&body body)
  `(with-database (*default-database* *conn-spec* :pool t :database-type *database-type*)
     ,@body))

(connect *conn-spec* :database-type *database-type* :if-exists :old)

(setf *default-caching* nil)

(locally-enable-sql-reader-syntax)

(defmacro when-valid ((&key userid articleid targetid ip) &body body)
  `(and (or (not ,userid) (and (get-user ,userid)
                              (not (neuterd ,userid))))
        (or (not ,targetid) (and (get-user ,targetid)
                                (not (neuterd ,targetid))))
        (or (not ,articleid) (get-article ,articleid))
        (or (not ,ip) (not (neuterd ,ip)))
        (progn ,@body)))
            
;;--------------------------- users ----------------------------
(defun user-pass (sn)
  (car (select [password] :from [users] :where [= sn [screenname]] :flatp t)))

(defun get-user (name-or-id)
  (typecase name-or-id
    (string (car (select 'user :where [= (string-downcase name-or-id) [lower [screenname]]] :flatp t )))
    (integer (car (select 'user :where [= name-or-id [id]] :flatp t )))))

(defun valid-login-p (sn pass)
  "Returns user id if the user's password is correct and NIL otherwise"
  (and sn pass
       (car (select [id] :from [users]
                    :where [and [= [lower [screenname]] (string-downcase sn)]
                                [= [password] pass]]
                    :flatp t))))


(defun valid-user-p (name-or-id &key return-sn)
  (typecase name-or-id
    (string (car (select (if return-sn [screenname] [id])
                         :from [users]
                         :where [= (string-downcase name-or-id) [lower [screenname]]]
                         :flatp t )))
    (integer (car (select (if return-sn [screenname] [id])
                          :from [users]
                          :where [= name-or-id [id]]
                          :flatp t )))))

(defun fake-user-p (name-or-id)
  (let ((id (valid-user-p name-or-id)))
    (and id (< id 0)))) 

(defun change-fake (name &optional newname)
  (let ((newname (or newname name)))
    (if (valid-user-p newname)
        (change-fake name (concatenate 'string newname "2"))
        (update-records [users] :attributes '(screenname) :values (list newname)
                        :where [= [lower [screenname]] (string-downcase name)]
                        ))))

(defun add-user-sql (name email pass ip &optional neg)
  "Adds a user to the database. Updates the userid sequences. neg
  indicates whether the userid should be negative (not a
  registered user)"
 (let ((id (if neg
                (- (sequence-next[userid]))
                (sequence-next[userid]))))
    (insert-records :into [users] :values (list id name email pass 0 [current_timestamp] ip))
    id))


(defun add-user (name email pass ip &optional fake)
  "adds a user to the database - this is for real users and the
  'fake' users by alexis and steve"
  ;when someone tries to register - check for a fake username
  (cond ((and fake (not (valid-user-p name)))
         (add-user-sql name email pass ip t))
        ((and (not fake) (fake-user-p name))
         (change-fake name)
         (add-user name email pass ip))
        ((valid-user-p name) (signal 'user-exists :username name))
        (t (add-user-sql name email pass ip))))

;;--------------------------- article --------------------------
(defun get-article-sn (id-or-url)
  (when id-or-url
    (typecase id-or-url
      (integer (car (select 'article-with-sn :where [= id-or-url [id]] :flatp t)))
      (string (car (select 'article-with-sn :where [= id-or-url [url]] :flatp t))))))

(defun get-article (id-or-url)
  (when id-or-url
    (typecase id-or-url
      (integer (car (select 'article :where [= id-or-url [id]] :flatp t)))
      (string (car (select 'article :where [= id-or-url [url]] :flatp t))))))

(defun insert-article (title url submitter ip &optional fuser)
  "Insert an article into the datebase and give user credit for
  it. If the artciles already exists, boost the orig submitter's
  karma."
  (and title url submitter ip (not (article-id-from-url url))
       (progn
         ;;handle fake user names
         (when fuser
           (if (fake-user-p fuser)
               (setf submitter (valid-user-p fuser))
               (progn
                 (when (not (valid-user-p fuser))
                   (add-user fuser [null] [null] ip t)
                   (setf submitter (valid-user-p fuser))))))
         ;;add http:// to urls if required
         (when-valid (:userid submitter :ip ip)
           (setf url (add-http url))
           (let ((article (or (get-article (article-id-from-url url))
                              (make-instance 'article :id (sequence-next "articleid") :url url
                                             :title title :date (get-time) :submitterid submitter))))
             (update-records-from-instance article)
             (like-and-mod submitter (article-id article) t ip)
             article)))))

(defun remove-article (userid articleid)
  (ignore-errors
    (delete-records :from [articles]
                    :where [and [= [id] articleid]
                                [= [submitter] userid]])))

;;--------------------------- neuter ---------------------------
(defun neuterd (id-or-ip)
  (typecase id-or-ip
    (string (car (select 'neuter :where [= id-or-ip [ip]] :flatp t)))
    (integer (car (select 'neuter :where [= id-or-ip [userid]] :flatp t)))))
  
;;------------------------- options ----------------------------
(defun get-user-options (userid)
  (or (car (select 'options :where [= userid [userid]] :flatp t))
      (make-instance 'options :userid userid :promoted t :demoted nil :numsites 25 :visible nil :frame nil)))

(defun profile-visible (userid)
  (options-visible (get-user-options userid)))

;;---------------------------- sessions ------------------------------
(defun session-uid (iden)
  (car (select [userid] :from [sessions] :where [= iden [iden]] :flatp t )))    

(defun remember-session-sql (id iden &optional ip)
  "Erase an old session for this userid, and add a new one"
  (ignore-errors
    (insert-records :into [sessions]
                    :av-pairs `(([userid] ,id) ([iden] ,iden) ([ip] ,ip) (date ,[current_date])))))

(defun remove-old-sessions ()
  (delete-records :from [sessions]
                  :where [< [date] [- [current_date]
                                      (sql-expression :string "interval '2 weeks'")]]))

;;----------------------------- mod-user -------------------------------
(defun check-and-mod-user (userid articleid ip amount)
  (let ((article (get-article articleid)))
    (and (not (= userid (article-submitterid article))) ;can't mod yourself
         (time< (time- (get-time) *mod-window*) ;only mod within 3 days of submission
                (article-date article))
         (> (or (user-karma (get-user userid)) 0)
            *min-karma*)
         (mod-user userid (article-submitterid article) articleid ip amount))))
         
(defun get-mod-user (userid targetid articleid)
  (car (select 'moduser :where [and [= [userid] userid]
                                    [= [target] targetid]
                                    [= [article] articleid]]
               :flatp t)))

(defun mod-user (userid targetid articleid ip amount)
  (when (and userid targetid articleid ip amount)
    (let ((moduser (or (get-mod-user userid targetid articleid )
                       (make-instance 'moduser :userid userid :targetid targetid :articleid articleid))))
      (log-message* "MOD-USER: userid: ~a target: ~a article: ~a ip: ~a amount: ~a"
                    userid targetid articleid ip amount)
      (setf (moduser-amount moduser) amount
            (moduser-date moduser) (get-time)
            (moduser-ip moduser) ip)
      (update-records-from-instance moduser))))


;;--------------------------- mod-article -----------------------------
(defun ip-modded-site (ip articleid)
  (car (select 'modarticle :where [and [= ip [ip]] [= articleid [article]]] :flatp t)))

(defun check-and-mod-article (userid articleid ip amount)
  (mod-article userid articleid ip amount))

(defun get-mod-article (userid articleid)
  (car (select 'modarticle :where [and [= [userid] userid]
                                       [= [article] articleid]]
               :flatp t)))

(defun mod-article (userid articleid ip amount)
  (and userid articleid ip amount
       (let ((modarticle (or (get-mod-article userid articleid)
                             (make-instance 'modarticle :userid userid :articleid articleid))))
         (log-message* "MOD-ARTICLE: userid: ~a article: ~a ip: ~a amount: ~a"
                     userid articleid ip amount)
         (setf (modarticle-amount modarticle) amount
               (modarticle-ip modarticle) ip
               (modarticle-date modarticle) (get-time))
         (update-records-from-instance modarticle))))

;;--------------------------- click on a link ----------------------------
(defun view-link (userid articleid ip)
  (and articleid ip
       (when-valid (:userid userid :articleid articleid :ip ip)
         (let ((click (make-instance 'click :userid userid :articleid articleid :ip ip)))
           (log-message* "CLICK user: ~a article: ~a ip: ~a" userid articleid ip)
           (update-records-from-instance click)))))


(defun user-clicked-p (userid articleid)
  (not (null (select 1
                     :from [clicks]
                     :where [and [= userid [userid]]
                                 [= articleid [article]]]
                     :limit 1))))
      
;;--------------------------- like-site ---------------------------
(defun get-like-site (userid articleid)
  (car (select 'like :where [and [= userid [userid]] [= articleid [article]]] :flatp t)))

(defun like-site (userid articleid liked)
  "Inserts or updates a user's liking of a site."
  (log-message* "LIKE user: ~a article: ~a like: ~a" userid articleid liked)
  (and userid articleid
       (let ((like (or (get-like-site userid articleid)
                       (make-instance 'like :userid userid :articleid articleid))))
         (setf (like-date like) (get-time)
               (like-like like) liked)
         (update-records-from-instance like))))

(defun unlike-site (userid articleid)
  (and userid articleid
       (progn
         (log-message* "UNLIKE user: ~a article: ~a" userid articleid)
         (when-bind (like (get-like-site userid articleid))
           (delete-instance-records like)))))

(defun like-site-user (userid articleid)
  (when-bind (like (get-like-site userid articleid))
    (if (like-like like) :like :dislike)))

(defun like-and-mod (userid articleid liked ip)
  (when-valid (:userid userid :articleid articleid :ip ip)
    (like-site userid articleid liked)
    (check-and-mod-article userid articleid ip (if liked 1 -1))
    (check-and-mod-user userid articleid ip (if liked 1 -1))))

(defun unlike-and-mod (userid articleid ip)
  (when-valid (:userid userid :articleid articleid :ip ip)
    (unlike-site userid articleid)
    (check-and-mod-article userid articleid ip 0)
    (check-and-mod-user userid articleid ip 0)))

;;-------------------------- aliases -------------------------------
(defun get-alias (userid name)
  (car (select 'alias :where [and [= [userid] userid] [= name [name]]] :flatp t)))

(defun set-alias (userid name val)
  (and userid (> (length name) 0) (> (length val) 0)
       (let ((alias (or (get-alias userid name)
                        (make-instance 'alias :userid userid))))
         (setf (alias-name alias) name
               (alias-val alias) val)
         (update-records-from-instance alias))))

(defun remove-alias (userid name)
  (and userid name
       (when-bind (alias (get-alias userid name))
         (delete-instance-records alias))))
       
(defun basic-info (sn)
  (car (select [karma] [signupdate]
               :from [users]
               :where [and [= (string-downcase sn) [lower [screenname]]]]
               :result-types '(t :int) :flatp t )))

(defun user-stats (sn-or-id)
  (let ((id (valid-user-p sn-or-id)))
    (list
     (car (select [count [id]] :from [articles]
                  :where [= [submitter] id]
                  :flatp t 
                  :result-types '(t)))
     (car (select [count [article]] :from [like_site]
                  :where [and [= id [userid]]
                              [= [liked] [true]]]
                  :flatp t 
                  :result-types '(t)))
     (car (select [count [article]] :from [like_site]
                  :where [and [= id [userid]]
                              [= [liked] [false]]]
                  :flatp t 
                  :result-types '(t))))))

(defun user-email (name-or-id)
  (let ((id (valid-user-p name-or-id)))
    (car (select [email] :from [users] :where [= [id] id] :flatp t ))))

(defun change-password (id oldpass newpass)
  (when (select 1 :from [users] :where [and [= [id] id] [= [password] oldpass]])
    (update-records [users]
                    :av-pairs `((password ,newpass))
                    :where [= [id] id])
                    t))
      
(defun user-from-email (email)
  (car (select [id] :from [users]
               :where [= (string-downcase email) [lower [email]]]
               :flatp t )))

(defun change-email (id email)
  (if (valid-user-p id)
      (update-records [users]
                      :av-pairs `((email ,email))
                      :where [= [id] id]
                      )))

(defun karma (id)
  (or 
   (car (select [karma] :from [users] :where [= id [id]] :flatp t  :result-types '(:int)
                ))
   0))
  

(defun login-from-email (email)
  (car (select [screenname] [password]
               :from [users]
               :where [= (string-downcase email) [email]]
               :flatp t )))

;;top submitters
(defun top-submitters(num timespan)
  (if (member timespan '(:day :week))
      (select [screenname] [users karma] [sum [amount]] :from '([users] [mod_user])
              :where  [and [= [target] [id]]
                           [> [karma] 0]
                           [> [mod_user date]
                             [- [current_timestamp]
                                (case timespan
                                  (:day (sql-expression :string "interval '1 day'"))
                                  (:week (sql-expression :string "interval '1 week'")))]]]
              :group-by (sql-expression :string "screenname, users.karma")
              :order-by `((,[sum [amount]] desc))
              :limit num)
      (select [screenname] [karma] :from [users]
              :where [> [karma] 0]
              :order-by `((,[karma] desc))
              :limit num)))

  
(defun valid-email (userid ip dest)
  (and userid ip dest
       (< (car (select [count [userid]] :from [emails]
                       :where [and [or [= userid [userid]]
                                       [= ip [ip]]]
                                   [> [date] [- [current_timestamp]
                                                (sql-expression :string "interval '1 day'")]]]
                       :flatp t ))
          *max-emails*)))

(defun email-sent (userid articleid ip dest)
  (insert-records :into [emails]
                  :av-pairs `((userid ,userid)
                              (articleid ,articleid)
                              (ip ,ip)
                              (dest ,dest)
                              (date ,[current_timestamp]))
                  ))


                     

(defun get-all (userid)
  (mapcar #'length
          (list (select [*] :from [like_site] :where [= [userid] userid])
                (select [*] :from [closed_sites] :where [= [userid] userid])
                (select [*] :from [saved_sites] :where [= [userid] userid])
                (select [*] :from [clicks] :where [= [userid] userid]))))
