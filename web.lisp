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

(defparameter *num-submitters* 8)
(defparameter *SESSION-MAX-TIME* 86400)
(defparameter *default-site-num* 25)
(defparameter *default-handler* 'default-handler)
(defparameter *recent-size* 5)

(defparameter *default-options* (make-instance 'options :numsites 25))

(defparameter *docklet-link* "javascript:location.href=\"http://reddit.com/submit?url=\"+encodeURIComponent(location.href)+\"&amp;title=\"+encodeURIComponent(document.title)")
(defparameter *docklet-onclick* "window.alert(\"Drag this link to your toolbar or right-click and choose Add to Favorites.\"); return false")

(defparameter *rewrite-for-session-urls* nil)

(defmacro with-html (&body body)
  `(with-html-output-to-string (*standard-output* nil :prologue t :indent nil)
     ,@body))

;;TODO fix multiple eval of params
(defmacro with-parameters (params &body body)
  `(let (,@(mapcar  (lambda (x) `(,(first x) (or (post-parameter ,(second x))
                                                 (get-parameter ,(second x))))) params))
     ,@body))

(defparameter *callbacks* (make-hash-table :test 'equal))

(defmacro define-callback (name args &body body)
  `(setf (gethash ,(string-downcase (string name)) *callbacks*)
         (lambda ()
           (let (,@(mapcar (lambda (x) `(,x (or (post-parameter ,(string-downcase (string x)))
                                                (get-parameter ,(string-downcase (string x))))))
                           args))
             ,@body))))

(defmacro pbox (name &body body)
  `(with-html-output (*standard-output*)
     (:div :class "pboxwrap"
           (:table :class "pbox"
                   (:tr (:td :nowrap t :class "pboxhead" ,name))
                   (:tr (:td :class "pboxbody" ,@body))))))

(defmacro hbar (text)
  `(with-html-output (*standard-output*)
     (:div :class "headbar" (:span :style "font-weight: bold;"  (esc ,text)))))

(defmacro prof-head (head)
  `(with-html-output (*standard-output*)
     (:tr (:td :nowrap t :colspan "3" :class "headbar" (:h1 :class "nomargin" ,head)))))

(defun check-parameters ()
  (with-parameters ((action "action"))
    (when-bind (fn (gethash action *callbacks*))
      (funcall fn))))

(defun ajax-op ()
  (with-parameters ((action "action"))
    (when-bind (fn (gethash action *callbacks*))
      (funcall fn))))

(defun set-session-cookie (iden &optional mem)
  (log-message* "set cookie to: ~a" iden)
  (set-cookie "reddit-session" :value iden
              :path "/"
              :expires (when mem (2weeks))))

(defun reset-session-cookies ()
  (set-cookie "mod" :path "/" :value "")
  (set-cookie "click" :path "/" :value "")
  (set-cookie "hide" :path "/" :value ""))

(defun check-cookies ()
  "Create a new session. Check for an existing session cookie,
  create one if it doesn't exists."
  (start-session)
  ;;see if the user has a previous session
  (with-web-db
    (let ((session-iden (cookie-in "reddit-session")))
      (when (and session-iden
                 (not (string= session-iden ""))
                 (not (session-value :user-id)))
        (log-message* "cookies is: ~a" session-iden)
        (when-bind (id (valid-cookie session-iden))
          (setf (session-value :user-id) id))))
    (reset-session-cookies)
    ;;user data needs to be reloaded because of karma change
    ;;options needs to be reloaded because the user options
    ;;object isn't updated when the options change
    (if (uid)
        (with-accessors ((userobj user-obj)
                         (options user-options)) (info)
          (update-instance-from-records userobj)
          (update-instance-from-records options)))))
  
(defun uid ()
  (and (ignore-errors *session*)
       (session-value :user-id)))

(defun info ()
  (get-info (uid)))

(defun logged-in-p ()
  (uid))

(defun options ()
  (or (when-bind (info (info))
        (user-options info))
      *default-options*))
  
(defun userobj ()
  (when-bind (info (info))
    (user-obj info)))

(defmacro with-main ((&key (menu "empty") (right-panel) (rss-url)) &body body)
  `(with-html
     (:html 
      (:head 
       (:meta :http-equiv "Content-Type" :content "text/html; charset=UTF-8")
       (:title "reddit - what's new online")
       (:script :src "/static/prototype.js" :language "javascript" :type "text/javascript" "")
       (:script :language "javascript" (str (if (logged-in-p) "var logged = true" "var logged= false")))
       (:script :src "/static/logic.js" :language "javascript" :type "text/javascript" "")
       (:link :rel "stylesheet" :href "/static/styles.css" :type "text/css")
       (:link :rel "shortcut icon" :href "/static/favicon.ico")
       (when ,rss-url
         (htm (:link :rel "alternate" :type "application/rss+xml" :title "RSS" :href ,rss-url))))
      ;;(htm (:link :rel "alternate" :type "text/xml" :title "RSS" :href ,rss-url))))
      (:body
       (:table :id "topbar" :cellpadding "0"
               (:tr 
                (:td :rowspan "2" (:a :href "/"  (:img :style "vertical-align: bottom; border: 0" :src "/static/redditheader.gif")))
                (:td :colspan "2" :width "100%" :class "topmenu menu" (menu-panel)))
               (:tr (:td :valign "bottom" :width "100%" (:div :id "topstrip" ,menu))
                    (:td :valign "bottom" :nowrap t (search-area))))
       (:div :id "right" ,right-panel)
       (:div :id "main" ,@body)
       (when ,rss-url
         (htm
          (:div :id "footer" (str "A") (:a :class "feed" :href ,rss-url "FEED") (str "is available."))))))))

(defmacro reddit-page ((&key (cache-key nil) (exp 0) (menu "empty") require-login right-panel rss-url) &body body)
  (let ((ck (gensym)))
    `(progn
       (check-cookies)
       (check-parameters)
       (let ((,ck ,cache-key))
         (if (and ,require-login
                  (not (logged-in-p)))
             (with-main (:menu ,menu :right-panel ,right-panel :rss-url ,rss-url)
               (:p :class "error" "please log in before continuing"))
             (if ,ck
                 (cached (,ck ,exp) (with-main (:menu ,menu :right-panel ,right-panel :rss-url ,rss-url) ,@body))
                 (with-main (:menu ,menu :right-panel ,right-panel :rss-url ,rss-url) ,@body)))))))

(defmacro idstr (name)
  `(format nil ,(conc name "~a") id))

(defun redirect-url (url)
  (setf (header-out "Location")
        url
        (return-code *reply*)
        +http-moved-permanently+)
  (throw 'tbnl-handler-done nil))

(defun user-link (name)
  (with-html-output (*standard-output*)
    (:a :href (conc "/user/" name) (esc name))))

(defun search-area ()
  (with-parameters ((query "q"))
    (with-html-output (*standard-output*)
      (:form :class "nomargin" :style "margin-left: 10px" :action "/search" :method "GET"
             (:input :class "txt" :style "vertical-align: bottom" :type "text" :name "q" :value (str (esc-quote query)))
             (:button :class "btn" :type "submit" "search")))))

(define-callback submit (id url title fuser save to message)
  (let ((id (or (sanitize id 'int)
                (article-id-from-url url))))
    (with-web-db
      ;;submit
      (when (and url title (not id))
        (when-bind (article (insert-article title url (uid) (session-remote-addr *session*) fuser))
          (log-message* "SUBMITTED: ~a" (article-title article))
          (when (and save (info))
            (setf (user-saved (info) (article-id article)) t)
            (save-site (uid) (article-id article)))
          (setf (user-liked (info) (article-id article)) :like)
          (ac-update *cached-new*)
          (setf id (article-id article)
                (session-value :submitted) t)))
      ;;recommend
      (when (and id (> (length to) 0))
        (when-bind* ((info (get-info (uid)))
                     (to (decode-aliases to info))
                     (email (user-emai (userobj)))) 
          (log-message* "EMAIL: ~a" to)
          (send-recommendation (uid) id (session-remote-addr *session*) to email
                               (and (> (length message) 0) (shorten-str message 500)))
          (setf (session-value :sent) t))))))

(defun login (user pass &optional mem)
  (with-web-db 
    (when-bind (id (valid-login-p user pass))
      (setf (session-value :user-id) id)
      (set-session-cookie (cookie-str user pass) mem)
      id)))

(define-callback register (user pass mem)
  (with-web-db
    (with-html-output (*standard-output*)
      (and user pass
           (let ((userid (valid-user-p user)))
             (if (and userid (not (fake-user-p user)))
                 (format nil "baduser")
                 (progn
                   (log-message* "REGISTER: ~a" user)
                   (add-user user nil pass (and *session* (session-remote-addr *session*)))
                   (login user pass mem)
                   (htm (format nil "~a (~a)" (user-name (userobj)) (user-karma (userobj)))))))))))


(define-callback login (user pass mem)
  (with-html-output (*standard-output*)
    (if (login user pass mem)
        (htm (format nil "~a (~a)" (user-name (userobj)) (user-karma (userobj))))
        (htm (str "invalid")))))

(define-callback logout ()
  (with-html
    (log-message* "LOGOUT: ~a" (uid))
    (remove-info (uid))
    (setf (session-value :user-id) nil)
    (set-session-cookie nil)))


(define-callback options (pro dem vis limit frame)
  (with-web-db
    (let ((options (options)))
      (with-accessors ((v options-visible)
                       (p options-promoted)
                       (d options-demoted)
                       (n options-numsites)
                       (u options-userid)
                       (f options-frame)) options
        (setf v (not (null vis))
              p (not (null pro))
              d (not (null dem))
              n (sanitize limit 'int)
              u (uid)
              f (not (null frame)))
        (update-records-from-instance options)))))

(define-callback frame (frame)
  (with-web-db
    (ignore-errors
      (when-bind (options (options))
        (setf (options-frame options) (not (null frame)))
        (update-records-from-instance options)))))

(define-callback sendpass (email)
  (with-web-db
    (let ((info (login-from-email email)))
      (when info
        (send-login-info email (first info) (second info))))))

(defun options-panel ()
  (let ((options (session-value :display-opts)))
    (pbox "display"
      (:form :method "get" :action (script-name) :class "nomargin"
             (:input :type "hidden" :name "action" :value "options")
             (:table :style "border-collapse: collapse: cell-padding-top: 3px;" :width "100%"
                     (when (logged-in-p)
                       (htm
                        (:tr (:td (:input :class "check" :type "checkbox" :name "pro" :checked (first options)))
                             (:td :nowrap t "promoted sites"))
                        (:tr (:td (:input :class "check" :type "checkbox" :name "dem" :checked (second options)))
                             (:td :nowrap t "demoted sites"))
                        (:tr (:td (:input :class "check" :type "checkbox" :name "hidden" :checked (third options)))
                             (:td :nowrap t "hidden sites"))))
                     (:tr (:td (:input :class "check" :type "checkbox" :name "24hrs" :checked (fourth options)))
                          (:td :nowrap t "from today"))
                     (:tr (:td :colspan "2" (:select :name "limit"
                                                     (:option :selected (eql (fifth options) 10) :value "10" "10")
                                                     (:option :selected (eql (fifth options) 25) :value "25" "25")
                                                     (:option :selected (eql (fifth options) 50) :value "50" "50")) " sites"))
                     (:tr (:td :colspan "2" :align "center" (:input :class "btn" :type "submit" :value "Apply"))))))))

(defun login-panel ()
  (pbox "login/register"
    (:form :id "logform" :class "nomargin"
           (:table :style "border-collapse: collapse"
                   (:tr (:td :colspan "2"  "username:"))
                   (:tr (:td :colspan "2" (:input :id "loguser" :class "txt" :name "user" :type "text" :size 15)))
                   (:tr (:td :colspan "2" "password:"))
                   (:tr (:td :colspan "2" (:input :id "logpass" :class "txt" :name "pass" :type "password" :size 15)))
                   (:tr (:td :colspan "2" (:input :id "logmem" :type "checkbox" :name "mem" "remember me")))
                   (:tr (:td :colspan "2" (:span :id "logerror" :class "error" "")))
                   (:tr (:td (:input :id "logbtn" :class "btn" :type "submit" :value "Login" :onclick "login(); return false"))
                        (:td (:input :class "btn" :type "submit" :value "Register" :onclick "register(); return false")))
                   (:tr (:td :nowrap t :colspan "2" :align "center" :class "little" (:a :href "/password" "what's my password?")))))))


(defun right-panel-main ()
  (with-html-output (*standard-output*)
    (unless (logged-in-p) (login-panel))))

(define-callback uemail (email)
  (with-web-db
    (with-html-output (*standard-output*)
      (if (user-from-email email)
          (htm (format nil "inuse"))
          (progn
            (let ((user (userobj)))
              (setf (user-emai user) email)
              (update-records-from-instance user)))))))

(define-callback upass (oldpass newpass)
  (with-web-db
    (with-html-output (*standard-output*)
      (when (change-password (uid) oldpass newpass)
        (htm (format nil "update"))))))

(define-callback delete (id)
  (with-web-db
    (with-html-output-to-string (*standard-output* nil)
      (remove-article (uid) id))))

(define-callback close (id)
  (with-web-db
    (with-html-output-to-string (*standard-output* nil)
      (when-bind* ((id (sanitize id 'int))
                   (info (info)))
        (if (user-closed info id)
            (progn
              (setf (user-closed info id) nil)
              (unclose-site-sql (uid) id))
            (progn
              (setf (user-closed info id) t)
              (close-site-sql (uid) id)))))))

(define-callback mod (id dir)
  (let ((id (sanitize id 'int))
        (dir (sanitize dir 'int)))
    (when-bind (info (info))
      (if (zerop dir)
          (progn
            (setf (user-liked info id) nil)
            (unlike-and-mod (uid) id (session-remote-addr *session*)))
          (progn
            (setf (user-liked info id) (if (plusp dir) :like :dislike))
            (like-and-mod (uid) id (plusp dir) (session-remote-addr *session*)))))))
   
(define-callback save (id)
  (with-web-db
    (when-bind* ((id (sanitize id 'int))
                 (info (info)))
      (unless (user-saved info id)
        (setf (user-saved info id) t)
        (save-site (uid) id)))))

(define-callback unsave (id)
  (with-web-db
    (when-bind (info (info))
      (setf (user-saved info id) nil)
      (unsave-site (uid) (sanitize id 'int)))))

(define-callback checkurl (url)
  (let ((at (check-url url)))
    (with-html-output-to-string (*standard-output* nil)
      (when at (str at)))))

(define-callback ualias (name val)
  (with-web-db
    (when-bind (info (info))
      (if val
          (progn
            (setf (user-alias info name) val)
            (set-alias (uid) name val))
          (progn
            (remhash name (user-info-alias info))
            (remove-alias (uid) name))))))

(defun site-link (id title url &optional clicked)
  (with-html-output (*standard-output*)
    (:a :id (idstr "title") :class (if clicked "title click" "title norm")
        :href url
        :onmousedown (makestr "return rwt(this," id ")") 
        :onrightclick (makestr "return rwt(this," id ")") 
        (str (escape-string-minimal title)))
    (:span :class "little" (fmt " (~a)" (tl-domain url)))))

(defun save-link (id saved)
  (with-html-output (*standard-output*)
    (if saved
        (htm (:a :class "bylink" :href (format nil "javascript:unsave(~a)" id) "unsave"))
        (htm (:a :class "bylink" :href (format nil "javascript:save(~a)" id) "save")))))

(defun hide-link (id closed)
  (with-html-output (*standard-output*)
    (if closed
        (htm (:a :class "bylink" :href (format nil "javascript:hideSite(~a)" id)  "unhide"))
        (htm (:a :class "bylink" :href (format nil "javascript:hideSite(~a)" id) "hide")))))

(defun print-articles (articles &optional (offset 0) savepage draw-close (draw-number t) (draw-share t))
  (with-html-output (*standard-output*)
    (loop for article in articles
       for x = (1+ offset) then (1+ x) do
       (with-accessors ((id article-id)
                        (title article-title)
                        (url article-url)
                        (pop article-pop)
                        (date article-date)
                        (subid article-submitterid)
                        (sn article-sn)) article
         (let* ((info (info))
                (clicked (and info (user-clicked info id)))
                (mod (and info (user-liked info id)))
                (closed (and info (user-closed info id)))
                (saved (and info (user-saved info id))))
           (htm 
            (:tr :id (idstr "site")
                 (if draw-number
                     (htm 
                      (:td :valign "top" :class "numbercol" :rowspan "2" (fmt "~a." x)))
                     (htm (:td :rowspan "2")))
                 (:td :valign "top" :rowspan "3" (button-area mod id))
                 (:td :colspan "2" :id (idstr "titlerow")
                      :class "evenRow" (site-link id title url clicked)))
            (:tr
             (:td :valign "top" :class "wide little"
                  (:span :id (idstr "score") (fmt "~a point~:p" (or pop 0)))
                  (htm (fmt " posted ~a ago by " (age-str date)))
                  (user-link sn)
                  (fmt " ")
                  (when (logged-in-p)
                    (when (or savepage (not saved))
                      (htm (:span :id (idstr "save") (save-link id saved))))
                    (when draw-share
                      (htm (:a :href (makestr "/share?id=" id) :class "bylink" "share")))
                    (when draw-close (hide-link id closed))
                    (when (= subid (uid))
                      (htm (:span :id (idstr "delete")
                                  (:a
                                   :href (format nil "javascript:deleteSite(~a)" id)
                                   :class "bylink"
                                   "delete")))))))
            (:tr (:td :colspan "5" :class "spacing"))))))))

(defun expand-button (id)
  (with-html-output (*standard-output*)
    (:div :id (idstr "ex") :class "expand" :onclick (format nil "expand(~a)" id))))

(defun button-area (mod id)
  (with-html-output (*standard-output* nil)
    (:div :id (idstr "up")
          :class (if (eq mod :like) "arrow upmod" "arrow up")
          :onclick (makestr "javascript:mod("id", 1)") " ")
    (:div :id (idstr "down")
          :class (if (eq mod :dislike) "arrow downmod" "arrow down")
          :onclick (makestr "javascript:mod("id", 0)"))))

(defun site-table (articles limit offset nextoff &optional savepage draw-closed searchpage)
  (with-html-output (*standard-output* nil :indent t)
    (if articles
        (htm
         (:table :id "siteTable" ;;:border "1"
                 (print-articles articles offset savepage draw-closed)
                 (when (eql (length articles) limit)
                   (let ((params (if searchpage
                                     `(("offset" . ,nextoff)
                                       ("q" . ,(get-parameter "q")))
                                     `(("offset" . ,nextoff)))))
                     (htm
                      (:tr (:td :colspan "4" (:a :href (create-url (script-name) params)
                                                 "View More"))))))))
        (htm (:span :class "error" "There are no sites that match your request")))))

(defun front-page-site-table (sort)
  (with-web-db
    (with-parameters ((offset "offset"))
      (setf offset (or (sanitize offset 'int) 0))
      (multiple-value-bind (articles nextoff)
          (get-sites-user (uid) (options-numsites (options)) offset sort)
        (site-table articles (options-numsites (options)) offset nextoff nil t)))))

(defun search-site-table ()
  (with-parameters ((offset "offset")
                    (query "q"))
    (setf offset (or (sanitize offset 'int) 0))
    (with-web-db
      (multiple-value-bind (articles nextoff)
          (get-search-sites (uid) query (options-numsites (options)) offset)
        (site-table articles (options-numsites (options)) offset nextoff nil nil t)))))

(defun page-search ()
  (with-web-db
    (reddit-page (:menu (top-menu (browse-menu)) :right-panel (right-panel-main))
      (search-site-table))))
               
(defun draw-boxes (pop growth)
  (with-html-output (*standard-output* nil :indent t)
    (:table :class "popbox" (:tr (:td :class "poppop" :width  pop)
                                 (:td :class "popgrowth" :width  growth)))))

(defun page-password ()
  (reddit-page (:menu (top-menu (browse-menu))
                      :right-panel (unless (logged-in-p) (login-panel)))
    (:h2 "what's my password?")
    (:p "enter your email below to receive your login information")
    (:span :class "error" :id "status" "")
    (:form :id "passform"
           (:table
            (:tr (:td "email:") 
                 (:td (:input :type "text" :id "email"))
                 (:td (:input :type "submit" :class "btn" :value "email me"
                              :onclick "sendpass(); return false")))))))

(defun menu-panel ()
  (with-html-output (*standard-output*)
    (if (logged-in-p)
        (htm          
         (format t "~a (~a) |" (user-name (userobj)) (user-karma (userobj)))
         (:a :href (conc "/user/" (user-name (userobj))) "profile") (str "|"))
        (htm
         (str "want to join? register in seconds |")))
    (:a :href "/submit" "submit") (str "|")
    (:a :href "/help/help.html" "faq") (str "|")
    (:a :href "/blog/index.html" "blog") (str "|")
    (:a :href "mailto:feedback@reddit.com" "feedback")
    (when (logged-in-p)
      (htm (str "|") (:a :href "javascript:logout()" "logout")))))

(defun top-menu (menu &optional selected)
  (with-html-output (*standard-output*)
    (loop for (sym title url) in menu do
         (htm
          (:a :class (if (eql sym selected)
                         "sel-menu-item"
                         "menu-item")
              :href url
              (esc title))))))

(defun browse-menu ()
  (let ((default '((:front "hottest" "/hot")
                   (:new "newest" "/new")
                   (:pop "top all-time" "/pop")
                   (:topsub "top submitters" "/topsub"))))
    (if (logged-in-p)
        (append default '((:saved "saved" "/saved")))
        default)))

(defun contacts-table ()
  (with-html-output (*standard-output*)
    (:table
     :id "contactlst"
     (let ((aliases (user-info-alias (info))))
       (if aliases
           (maphash #'(lambda (name val)
                        (htm
                         (:tr (:td (:a :style "font-size: normal; color: #336699"
                                       :href "#" :onclick "sendrow(this); return false" "add"))
                              (:td (esc name)) (:td :width "100%" (esc val))
                              (:td (:a :href "#" :onclick "editrow(this); return false" "edit"))
                              (:td (:a :href "#" :onclick "removerow(this); return false" "delete")))))
                    aliases)
           "")))))
           
(defun bottom-submit ()
  (with-html-output (*standard-output*)
    (:div :id "sharetoggle" :class "menu collapse r" (:a :id "sharelink" :href "javascript:shareon()" "share"))
    (:div :id "share" :style "display: none"
          (:table
           (:tr
            (:td :align "right" "from")
            (:td (let ((email (user-emai (userobj))))
                   (if email
                       (htm (:span :id "email" :class "gray" (esc email)))
                       (htm (:span :class "error" "you will not be able to send email until you set your own email address by clicking the profile link at the top of the page"))))))
           (:tr 
            (:td :align "right" "to")
            (:td (:input :type "text" :id "to" :name "to" :size 60)))
           (:tr 
            (:td) (:td :id "contoggle" :class "menu collapse r" (:a :id "conlink" :href "javascript:contactson()" "contacts")))
           (:tr :id "contacts" :style "display: none"
                (:td)
                (:td (contacts-table)
                     (:span :class "menu" (:a :href "javascript:addrow()" "add contact"))))
           (:tr 
            (:td :valign "top" :align "right" "message")
            (:td (:textarea :id "personal" :name "message" :rows "2" :cols "60" "")))))
    (:div :style "margin: 10px 0 20px 0"
          (:button :class "btn" :type "submit" "send")
          (:span :id "status" :class "error" ""))
    (:span :class "menu" "submit and share links faster with the " (:a :href "/help/docklet.html" "spreddit docklet"))))

(defun page-submit ()
  (with-parameters ((id "id") (url "url") (title "title"))
    (reddit-page (:menu (top-menu (browse-menu)) :require-login t :right-panel (right-panel-main))
      (let ((id (or (sanitize id 'int)
                    (article-id-from-url url))))
        (htm
         (:script :src "/static/contacts.js" :language "javascript" :type "text/javascript" "")
         (:form
          :onsubmit "return chksub()" :action (script-name) :method "post" :class "meat"
          (:input :type "hidden" :name "action" :value "submit")
          (:input :type "hidden" :name "id" :value id)
          (let ((article (get-article-sn id)))
            (cond
              ;;invalid id
              ((and id (not article))
               (htm (:span :class "error" "that site does not exist")))
              ;;valid id - share
              (article
               (htm 
                (:h1 "share")
                (:span :class "error"
                       (str
                        (cond ((session-value :submitted)
                               (setf (session-value :submitted) nil)
                               "your submission was successful")
                              (url
                               "this site has already been submitted")
                              (t ""))))
                (when (session-value :sent)
                  (htm (:br) (:span :class "error" "your recommendations have been delivered"))
                  (setf (session-value :sent) nil))
                (:table
                 (print-articles (list article) 0 nil nil nil nil)))
               (bottom-submit))
              ;;no id - submit   page
              (t
               (htm
                (:h1 "submit")
                (:div :id "wtf"
                      (:table
                       (:tr (:td :align "right" "url")
                            (:td (:input :id "url" :name "url" :type "text" :value url :size 60))
                            (:td :id "urlerr" :class "error"))
                       (:tr (:td :align "right" "title")
                            (:td (:input :id "title" :name "title" :style (unless title "color: gray")
                                         :value (if title (esc-quote title) "Enter a title, or click submit to find one automatically.")
                                         :onfocus (unless title "clearTitle(this)") :type "text" :size 60))
                            (:td :id "titleerr" :class "error"))
                       (:tr (:td) (:td (:input :id "save" :name "save" :type "checkbox") "add to my saved sites")))))
               (bottom-submit))))))))))

(defun use-frame ()
  (and (uid)
       (ignore-errors
         (options-frame (options)))))
      
(defun load-link (id)
  (with-web-db
    (let ((article (get-article id)))
      (if article
          (progn
            (when-bind (info (info))
              (setf (user-clicked info (article-id article)) t))
            (view-link (uid) (article-id article) (ignore-errors (session-remote-addr *session*)))
            (if (use-frame)
                (reddit-frame article)
                (redirect-url (article-url article))))
          (reddit-page (:menu (top-menu (browse-menu)) :right-panel (right-panel-main))
            (:span :class "error" "that article does not exist."))))))

(defun viewlink ()
  (with-parameters ((id "id"))
    (load-link (sanitize id 'int))))

(defun lucky ()
  (let* ((n (random 10))
         (source (if (< n 5)
                     (get-articles 50 0 :front)
                     (get-articles 50 0 :new)))
         (filter (if (< n 8)
                     (lambda (a) (>= (article-pop a) 2))
                     #'identity))
         (info (info)))
    (article-id
     (or (and (uid) info
              (find-if #'(lambda (a) (and (funcall filter a)
                                          (not (user-clicked info (article-id a)))))
                       source))
         (elt source (random (length source)))))))

(defun page-lucky ()
  (load-link (lucky)))

(defun wrap-static-file (path)
  (reddit-page (:cache-key (unless (logged-in-p) (key-str (script-name)))
                           :exp 60
                           :menu (top-menu (browse-menu))
                           :right-panel (unless (logged-in-p) (login-panel))
                           :rss-url (and (string= path "/home/reddit/reddit/web/blog/index.html") "http://reddit.com/blog/atom.xml"))
    (with-open-file (in path :direction :input)
      (loop for line = (read-line in nil) 
         while line do
         (format t "~a~%" line)))))

(defun default-handler ()
  (let ((path (and (> (length (script-name)) 1)
                   (conc "/home/reddit/reddit/web/" (subseq (script-name) 1)))))
    (if (and path (probe-file path))
        (wrap-static-file path)
        (page-default))))



(macrolet ((page-main (name selected &optional rss-url)
             `(defun ,name ()
                (with-parameters ((offset "offset"))
                  (with-web-db
                    (reddit-page (:cache-key (unless (logged-in-p) (key-str ',name offset))
                                             :exp 60
                                             :menu (top-menu (browse-menu) ,selected)
                                             :right-panel (right-panel-main)
                                             :rss-url ,rss-url)
                      (front-page-site-table ,selected)))))))

  (page-main page-front :front "http://reddit.com/rss/hot")
  (page-main page-pop :pop "http://reddit.com/rss/pop")
  (page-main page-new :new "http://reddit.com/rss/new"))

(defun page-default ()
  (page-front))

(defun page-saved ()
  (if (logged-in-p)
      (reddit-page (:menu (top-menu (browse-menu) :saved))
        (with-web-db
          (profile-site-table (uid) :saved)))
      (page-default)))

(defun page-submitters ()
  (reddit-page (:cache-key (key-str (and (logged-in-p) (user-name (userobj)) "topsub"))
                           :exp 60
                           :menu (top-menu (browse-menu) :topsub)
                           :right-panel (unless (logged-in-p) (login-panel)))
    (with-web-db
      (let ((today (top-submitters *num-submitters* :day))
            (week (top-submitters *num-submitters* :week))
            (all (top-submitters *num-submitters* nil)))
        (htm
         (hbar "top submitters today")
         (:table
          (loop for (name karma change) in today do
               (htm (:tr (:td :class "black" (user-link name) (format t " (~a)" karma))
                         (:td (format t "+~a" change))))))
         (hbar "top submitters this week")
         (:table
          (loop for (name karma change) in week do
               (htm (:tr (:td :class "black" (user-link name) (format t " (~a)" karma))
                         (:td (format t "+~a" change))))))
         (hbar "top submitters all-time")
         (:table
          (loop for (name karma change) in all do
               (htm (:tr (:td :class "black" (user-link name) (format t " (~a)" karma)))))))))))

(defun page-blog ()
  (redirect "/blog/index.html"))

(defun page-help ()
  (redirect "/help/help.html"))

(defun page-test ()
  t)

(setq *dispatch-table*
      (nconc
       (list (create-static-file-dispatcher-and-handler
              "/favicon.ico"
              (make-pathname :directory "/home/reddit/reddit/web/" :name "favicon" :type "ico" :version nil
                             :defaults (load-time-value *load-pathname*))
              "image/x-icon"))
       (mapcar (lambda (args)
                 (apply #'create-prefix-dispatcher args))
               '(("/rss/new" rss-new)
                 ("/rss/hot" rss-hot)
                 ("/rss/pop" rss-pop)
                 ("/viewlink" viewlink)
                 ("/browse" page-default)
                 ("/submit" page-submit)
                 ("/hot" page-front)
                 ("/pop" page-pop)
                 ("/new" page-new)
                 ("/saved" page-saved)
                 ("/topsub" page-submitters)
                 ("/search" page-search)
                 ("/aop" ajax-op)
                 ("/test" page-test)
                 ("/logout" logout)
                 ("/share" page-submit)
                 ("/password" page-password)
                 ("/lucky" page-lucky)
                 ("/user/" page-user)
                 ("/toolbar" reddit-toolbar)))
       (list (create-static-file-dispatcher-and-handler
              "/blog/atom.xml" "/home/reddit/reddit/web/blog/atom.xml" "text/xml"))
       (mapcar (lambda (args)
                 (apply #'create-regex-dispatcher args))
               '(("/blog/.+" default-handler)
                 ("/blog/?" page-blog)
                 ("/help/.+" default-handler)
                 ("/help/?" page-help)))
       (list #'default-dispatcher)))
