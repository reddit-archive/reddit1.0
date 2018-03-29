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

(defun all-articles ()
  (query "select articles.id, url, title, pop, screenname from articles, users where articles.submitter = users.id"
          :result-types '(t t t t t)
          :field-names nil))

;;TODO consider defaults
(defun get-articles (&key (limit 25) offset sort-by time-frame threshold)
   (query
    (with-output-to-string (s)
     (format s "select articles.id, url, title, pop, sum(amount) as growth, screenname, articles.date from articles, mod_article, users where articles.id = mod_article.article and submitter = users.id")
     (when threshold (format s " and pop > ~a" threshold))
     (case time-frame
       (:day (format s " and articles.date > timestamp current_timestamp - interval '1 day'"))
       (:week (format s " and articles.date > timestamp current_timestamp - interval '1 week'")))
     (format s " group by articles.id, url, title, pop, screenname, articles.date")
     (case sort-by
       (:pop (format s " order by pop desc, articles.date desc"))
       (:growth (format s " order by growth desc, articles.date desc"))
       (t (format s " order by articles.date desc")))
     (when limit (format s " limit ~a" limit))
     (when offset (format s " offset ~a" offset)))
   :result-types '(t t t t t t t t)))


(defun mod-frame ()
  (with-parameters ((article "article"))
    (let ((url (article-url article)))
      (if url
          (with-html
              (:html
               (:frameset :rows "50, *"
                          (:frame :src (conc "/lisp/reddit/mod?article=" article) :scrolling "no")
                          (:frame :src url))))
          (redirect "/lisp/reddit")))))

(defun mod-panel ()
  (with-parameters ((article "article") (mod "mod"))
    (with-html 
        (if mod
            (progn
              (htm (:span "Your moderation has been recored"))
              (mod-article (user-val 'id) article mod))
            (htm (:form :method "get" :action (script-name)
                        (:table
                         (:tr (loop for x from -5 to 5 do
                                   (htm (:td :align "center" (str x))))
                              (:td :rowspan "2" (:input :type "submit" :value "Moderate"))
                              (:tr (loop for x from -5 to 5 do
                                        (htm (:td :align "center" (:input :type "radio" :name "mod" :value (str x)))))))
                         (:input :type "hidden" :name "article" :value article))))))))

(defun get-articles (userid limit offset  sort-by time-frame &optional threshold)
   (query
    (with-output-to-string (s)
     (format s "select articles.id, title, pop, sum(amount) as growth, screenname, seconds(articles.date), ~a"
             (if userid (format nil "mod_amount(~a, articles.id)" userid) 0))
     (format s "from articles, mod_article, users where articles.id = mod_article.article and submitter = users.id")
     (when threshold (format s " and pop > ~a" threshold))
     (case time-frame
       (:day (format s " and mod_article.date > current_timestamp - interval '1 day'"))
       (:week (format s " and mod_article.date > current_timestamp - interval '1 week'")))
     (format s " group by articles.id, url, title, pop, screenname, articles.date")
     (case sort-by
       (:pop (format s " order by pop desc, articles.date desc"))
       (:growth (format s " order by growth desc, articles.date desc"))
       (t (format s " order by articles.date desc")))
     (when offset (format s " offset ~a" offset))
     (when limit (format s " limit ~a" limit)))
   :result-types '(:int t t t t :int :int)))

(defun link-or-text (sym csym title link)
  (with-html-output (*standard-output*)
    (if (eql sym csym)
        (htm (esc title))
        (htm (:a :href link (esc title))))))

(defun mod-article-sql-str (user articleid ip amount)
  (with-output-to-string (s)
    (format s "insert into mod_article(~a, article, amount, effamount, date, ip) "
            (typecase user
              (string "sessionid")
              (integer "userid")))
    (format s "values ('~a', ~a, ~a, ~a, current_timestamp, '~a')"
            user articleid amount amount ip)))


                                     ;((= 1 (length (session-value 'recent))) "oneRow")
                                     ;((= x 0) "topRow")
                                     ;((< x (1- (length (session-value 'recent)))) "midRow")
                                     ;((= x (1- (length (session-value 'recent)))) "botRow"))
                                   ;"evenRow")

     (case time-frame
       (:day (format s " and mod_article.date > current_timestamp - interval '1 day'"))
       (:week (format s " and mod_article.date > current_timestamp - interval '1 week'")))
     (format s " group by articles.id, url, title, pop, screenname, articles.date")
     (case sort-by
       (:pop (format s " order by pop desc, articles.date desc"))
       (:growth (format s " order by growth desc, articles.date desc"))
       (t (format s " order by articles.date desc")))
     (when offset (format s " offset ~a" offset))
     (when limit (format s " limit ~a" limit)))

(defun topic-link (topic)
  (with-html-output(*standard-output*)
    (:a :class "userlink" :href (create-url "/" (replace-alist (get-parameters) `(("topic" . ,topic)))) (esc topic))))

(defun topics-panel ()
  (let ((topics (get-topics)))
    (pbox "Topics"
      (:table
       (loop for topic in topics do
            (htm
             (:tr (:td (topic-link topic)))))))))

(defun star-panel ()
  (with-html-output (*standard-output*)
    (dotimes (i 4)
      (:div 

(defun get-articles (userid limit offset sort-by)
   (query
    (with-output-to-string (s)
     (format s "select articles.id, title, url, pop, sum(amount) as growth, screenname, seconds(articles.date), ~a"
             (if userid (format nil "mod_amount(~a, articles.id)" userid) 0))
     (format s " from articles, mod_article, users")
     (format s " where articles.id = mod_article.article and submitter = users.id")
     ;default is growth
     ;(when topic
       ;(format s " and topics.name = '~a'" topic))
     (case sort-by
       (:pop)
       (:date)
       (t (format s " and mod_article.date > current_timestamp - interval '1 day'")))
     (format s " group by articles.id, url, title, pop, screenname, articles.date")
     ;default is growth
     (case sort-by
       (:pop (format s " order by pop desc"))
       (:date (format s " order by articles.date desc"))
       (t (format s " order by growth desc, articles.date desc")))
     (when offset (format s " offset ~a" offset))
     (when limit (format s " limit ~a" limit)))
     :result-types '(:int t t t t t :int :int)))

(defun recent-articles ()
  (let ((articles (get-articles-lst (user-val 'id) (session-value 'recent))))
    (when articles
      (setf *test* (session-value 'recent))
      (with-html-output (*standard-output*)
        (:tr (:td "Recently Viewed:"))
        (print-articles articles t)))))

(defun add-recent (id)
  (if (session-value 'recent)
      (setf (session-value 'recent) (add-rlist id (session-value 'recent) *recent-size*))
      (setf (session-value 'recent) (list id))))

(defun remove-recent (id)
  (when (session-value 'recent)
    (setf (session-value 'recent) (remove id (session-value 'recent)))))

(defun table-link (sym csym url title)
  (with-html-output (*standard-output*)
    (if (or (and (null csym)
                 (eql sym :growth))
            (eql sym csym))
        (htm
         (esc title))
        (htm
         (:a :class "tablelink" :href url (esc title))))))

(defun table-controls (&optional (selected :front))
  (with-html-output (*standard-output*)
    (table-link :front selected "/hot" "Hottest")
    " | "
    (table-link :prom selected "/prom" "Recently Promoted")
    " | "
    (table-link :new selected "/new" "Newest")
    " | "
    (table-link :pop selected "/pop" "Most Popular (all-time)")))


(defun rate-area (modded id) 
  (with-html-output (*standard-output* nil :indent t)
    (:div :class "mod" (unless modded
                         (htm
                          (:a :href (create-url (script-name) (replace-alist (get-parameters)
                                                                             `(("action" . "mod")
                                                                               ("mod" . "cool")
                                                                               ("id" . ,id))))))))))
(define-callback mod (id mod)
  (let ((id (sanitize id 'int))
        (mod (sanitize mod 'sym '(:cool :uncool))))
    (when (and id mod)
      (moderate id mod)
      (redirect (script-name)))))

(defun moderate (id mod)
  (setf (session-value 'last-id) id)
  (let ((modamount (if (member (user-val 'id) '(0 2)) (+ (random 4) 2) 1)))
    (case mod
      (:cool (mod-article (user-val 'id) id modamount (session-remote-addr *session*))n
             (setf (gethash id (session-value :modded-articles)) modamount))
      (:uncool (mod-article (user-val 'id) id (- modamount) (session-remote-addr *session*))
               (setf (gethash id (session-value :modded-articles)) (- modamount))))))


(defun submit-panel ()
  (with-parameters ((url "url") (title "title") (ref "ref"))
    (with-html-output (*standard-output*)
      (typecase (session-value 'submit)
        (article-submitted 
         (htm (:p "Your submission was successful")
              (:p (site-link (id (session-value 'submit)) (title (session-value 'submit)) url)))
         (setf (session-value 'submit) nil))
        (article-exists
         (htm (:p :class "error" "That site has already been submitted")
              (:p (site-link (id (session-value 'submit)) (title (session-value 'submit)) url)))
         (setf (session-value 'submit) nil))
        (t
         (if (logged-in-p)
             (let ((ctitle (session-value 'submit)))
               (htm (:div :id "contentPanel"
                          (:form :method "post" :action (script-name)
                                 (:h2 "Submit")
                                 (:input :type "hidden" :name "action" :value "submit")
                                 (:input :type "hidden" :name "ref" :value (referer))
                                 (:table
                                  (when (member  (user-val 'id) '(0 2 64))
                                    (htm 
                                     (:tr (:td :align "right" "Screenname:")
                                          (:td (:input :name "fuser" :type "text" :size 60 :value (esc (user-val 'name)))))))
                                  (:tr (:td :align "right" "URL:")
                                       (:td (:input :name "url" :type "text" :value (esc (or url "")) :size 60)))
                                  (:tr (:td :align "right" "Title:")
                                       (:td (:input :name "title" :type "text" :value (esc (or 
                                                                                            (when ctitle (title ctitle))
                                                                                            title "")) :size 60)))
                                  (when ctitle
                                    (htm
                                     (:tr (:td) (:td :class "error" "Please verify this title, or enter one of your own"))))
                                        ;(:tr (:td :align "right" "Description:") (:td (:textarea :name "description" :rows 5 :cols 60 "")))
                                  (:tr (:td) (:td (:input :type "submit" :value "Submit")))))
                          (esc "Drag this link to your toolbar to submit links faster: ")
                          (:a :href "javascript:location.href=\"http://reddit.com/submit?url=\"+encodeURIComponent(location.href)+\"&amp;title=\"+encodeURIComponent(document.title)" 
                              :onclick "window.alert(\"Drag this link to your toolbar or right-click and choose Add to Favorites.\"); return false"  "post on Reddit")))
               (setf (session-value 'submit) nil))
             (htm (:p "Please log in before submitting."))))))))

(define-callback submit (url title fuser)
  (handler-bind ((check-article-title (lambda (c) (setf (session-value 'submit) c)))
                 (article-exists (lambda (c) (setf (session-value 'submit) c)))
                 (article-submitted (lambda (c) (setf (session-value 'submit) c))))
    (let ((userid (user-val 'id)))
      (when (or (not fuser)
                (member userid '(0 2 64)))
        (insert-article title url description userid (session-remote-addr *session*) fuser)))))


(defun req-site-info (userid siteid pro-p dem-p hidden-p)
  "Returns a list of the following properties: modded-p,
  saved-p, :good/:bad. If the site shouldn't be displayed,
  returns nil."
    (unless (and userid (site-closed-p-sql userid siteid))
      (let ((goodbad :good)) ;(if (< (site-prob userid siteid) *prob-threshold*)
                                        ;:bad :good)))
        (when (or (eql type :both)
                  (eql type goodbad))
          (list 
           (or (mod-amount-sql userid siteid) 0)
           (when (> userid -1)
             (site-saved-p-sql userid siteid))
           goodbad)))))


(defun print-articles (articles &optional (offset 0) savepage)
  (with-html-output (*standard-output*)
  (loop for (id title url pop submitter seconds modded saved closed goodbad) in articles
     for x = (1+ offset) then (1+ x) do
     (htm 
      (:tr :id (idstr "site")
           (:td :valign "top" :class "numbercol" (fmt "~a." x))
           (:td :rowspan "3" :valign "top" (rate-area modded id pop) (rate-area modded id pop t))
           (:td :colspan "2" :id (idstr "title") :class "evenRow"
                 ;(case goodbad
                   ;(:good (htm (:span :style "color:green" "*")))
                   ;(:bad (htm (:span :style "color:red" "*")))
                   ;(t (log-message* "~a" goodbad)))
                 (site-link id title url))
           (when (logged-in-p)
             (htm
              (:td :id (idstr "close") :class "evenRow"
                   :align "right"
                   :valign "top" (close-button id closed)))))
      (:tr (:td)
           (:td (expand-button id))
           (:td :valign "top" :class "wide little"
                (str " by ")  (user-link submitter) (fmt " ~a ago with " (age-str seconds))
                (:span :id (idstr "pop") (esc (pop-text pop))) (str " ")
                (when (and (logged-in-p)
                           (or savepage
                               (not saved)))
                  (htm (:span :id (idstr "save") (save-link id saved))))))
      (:tr (:td (:input :type "hidden" :id (idstr "descr")
                        :value (format nil "This is a lame description of ~a" (esc title))))
           (:td :colspan "3" :class "wide" :id (idstr "info")))
      (:tr (:td :colspan "3" :class "spacing"))))))


(defun page-test ()
  (let ((count (or (session-value :count) 0))
        (time (if (> (get-universal-time) (+ 10 (or (session-value :time) 0)))
                  (get-universal-time)
                  (session-value :time))))
  (handle-if-modified-since time)
  (setf (session-value :count) (1+ count))
  (with-html-output (*standard-output*)
    (setf (header-out "Last-Modified") (rfc-1123-date time))
    (setf (session-value :time) time)
    (format nil "compute time: ~a<br/>session-time: ~a<br/>header time: ~a<br/>count: ~a"
            (rfc-1123-date time) (rfc-1123-date (session-value :time))(header-in  "If-Modified-Since") count))))

(defun page-test ()
  (let ((count (or (session-value :count) 0))
        (time (if (> (get-universal-time) (+ 10 (or (session-value :time) 0)))
                  (get-universal-time)
                  (session-value :time))))
  (handle-if-modified-since time)
  (setf (session-value :count) (1+ count))
  (with-html-output (*standard-output*)
    (setf (header-out "Last-Modified") (rfc-1123-date time))
    (setf (session-value :time) time)
    (format nil "compute time: ~a<br/>session-time: ~a<br/>header time: ~a<br/>count: ~a"
            (rfc-1123-date time) (rfc-1123-date (session-value :time))(header-in  "If-Modified-Since") count))))


(defun get-articles (&optional (limit 25) (offset 0) (sort :front) (today nil))
  (select [articles id] [title] [url] [sum [amount]] [screenname] (sql-operation 'function "seconds" [articles date])
          :from '([articles] [users] [mod_article])
          :where [and [= [articles id] [article]]
                      [= [submitter] [users id]]
                      (if (eql sort :front) [> [pop] *min-front-page-pop*] t)
                      (if today [> [articles date] [- [current_timestamp] (sql-expression :string "interval '1 day'")]] t)]
          :group-by (sql-expression :string "articles.id, title, url, pop, screenname, articles.date")
          :order-by (case sort
                      (:pop `((,[sum [amount]] desc)))
                      (:new '(([articles date] desc)))
                      (:prom `((,[max [mod_article date]] desc)))
                      (t `((,[- [sum [amount]]
                                [/ (sql-operation 'function "seconds" [articles date])
                                   3600]] desc))))
          :offset offset
          :limit limit
          :result-type '(:int t t t t t)))

(defun profile-sites (userid limit offset display)
  "display can be :saved :hidden :submitted :promoted :demoted"
  (select [articles id] [title] [url] [sum [amount]] [screenname] (sql-operation 'function "seconds" [articles date])
          :from '([articles] [users] [mod_article])
          :where [and [= [articles id] [article]]
                      [= [users id] [submitter]]
                      (case display
                        (:saved [and [= userid [saved_sites userid]]
                                     [= [articles id] [saved_sites article]]])
                        (:hidden [and [= userid [closed_sites userid]]
                                      [= [articles id] [closed_sites article]]])
                        (:submitted [= [submitter] userid])
                        (:promoted [> [select [amount] :from [mod_article]
                                              :where [and [= [userid] userid]
                                                          [= [article] [articles id]]]
                                              :limit 1]
                                      
                                      0])
                        (:demoted [< [select [amount] :from [mod_article]
                                             :where [and [= [userid] userid]
                                                         [= [article] [articles id]]]
                                             :limit 1]
                                     0]))]
          :group-by (sql-expression :string "articles.id, title, url, pop, screenname, articles.date")
          :order-by '(([articles date] desc))
          :offset offset
          :limit limit
          :result-type '(:int t t t t t)))


function modup(id, up, down) {
    var ra = document.getElementById("rate" + id);
    ra.innerHTML = "<div class='arrow aup'></div>" +
        "<a class='arrow adown' href='javascript:moddown(" + id + "," + up + "," + down + ")'></a>";
    modsite(id, 1, up + 1, down);
}

function moddown(id, up, down) {
    var ra = document.getElementById("rate" + id);
    ra.innerHTML = "<a class='arrow aup' href='javascript:modup(" + id + "," + up + "," + down + ")'></a>" +
        "<div class='arrow adown'></div>";
    modsite(id, -1, up, down + 1);
}

function modsite(id, dir, up, down) {
    var pop = document.getElementById("pop" + id);
    var percent = Math.round(100 * up / (up + down));
    pop.innerHTML = "[" + percent + "% of " + (up + down) + "] ";
    pop.className = "little highlight";

    new Ajax.Request('/aop', {parameters: "action=mod&id="+id+"&dir="+dir});
}

function expand(id) {
    var info = document.getElementById("info" + id);
    var button = document.getElementById("ex" + id);
    info.appendChild(infoPanel(id));
    //info.className = "info wide";
    button.className = "collapse";
    button.onclick = function(){collapse(id)};
}

function collapse(id) {
    var info = document.getElementById("info" + id);
    var button = document.getElementById("ex" + id);
    info.innerHTML = "";
    //info.className = "wide"
    button.className = "expand";
    button.onclick = function(){expand(id)};
}

function infoPanel (id) {
    var descr = document.getElementById("descr" + id);
    var info = document.createElement("div");
    info.className = "info";

    var ddiv = document.createElement("div");
    ddiv.style.marginBottom = "4px";
    ddiv.innerHTML = descr.value;
    info.appendChild(ddiv);

    var ldiv = document.createElement("div");
    var links = new Array("edit title", "editTitle(" + id + ")",
                          "edit description", "editDesc(" + id + ")");
    for (i = 0; i < links.length; i++) {
        ldiv.appendChild(createLink(links[i], links[++i]));
        if (i < links.length - 1) ldiv.appendChild(document.createTextNode(" | "));
    }
    ldiv.className = "little gray";
    info.appendChild(ldiv);
    
    return info;
}

function createLink (text, fn) {
    var a = document.createElement("a");
    a.href = "javascript:" + fn;
    a.innerHTML = text;
    return a;
}

function editTitle (id) {
    var titlerow = document.getElementById("titlerow" + id);
    var title = document.getElementById("title" + id);
    if (!title) return;
    
    var oldtitle = title.innerHTML;
    oldtitles[id] = titlerow.innerHTML;
    titlerow.innerHTML = "";
    var titlebox = textBox(oldtitle);
    titlebox.style.marginRight = "3px";
    titlerow.appendChild(titlebox);

    var savebtn = button("save",
                         function () {
                             titlerow.innerHTML = oldtitles[id];
                             oldtitles[id] = null;
                             var title = document.getElementById("title" + id);
                             title.innerHTML = titlebox.value;
                         });
    savebtn.style.marginRight = "3px";

    var canbtn = button("cancel",
                        function () {
                            titlerow.innerHTML = oldtitles[id];
                            oldtitles[id] = null;
                        });
    titlerow.appendChild(savebtn);
    titlerow.appendChild(canbtn);
                             
}

function textBox(text) {
    var box = document.createElement("input");
    box.type = "text";
    box.value = text;
    box.size = 40;
    return box;
}

function button(text, fn) {
    var btn = document.createElement("input");
    btn.type = "submit";
    btn.value = text;
    btn.onclick = fn;
    btn.className = "btn";
    return btn;
}

function createCookie(name,value,days)
{
	if (days)
	{
		var date = new Date();
		date.setTime(date.getTime()+(days*24*60*60*1000));
		var expires = "; expires="+date.toGMTString();
	}
	else var expires = "";
	document.cookie = name+"="+value+expires+"; path=/";
}

function readCookie(name)
{
	var nameEQ = name + "=";
	var ca = document.cookie.split(';');
	for(var i=0;i < ca.length;i++)
	{
		var c = ca[i];
		while (c.charAt(0)==' ') c = c.substring(1,c.length);
		if (c.indexOf(nameEQ) == 0) return c.substring(nameEQ.length,c.length);
	}
	return null;
}

function eraseCookie(name)
{
	createCookie(name,"",-1);
}

(defun page-wtf ()
  (reddit-page (:menu (top-menu (browse-menu)) :right-panel (right-panel-main))
    (:div :class "meat"
          (with-parameters ((id "id"))
            ;;There has got to be a better way than (or blah '(nil nil))
            (destructuring-bind ((title url) others) (list (or (site-tl id) '(nil nil)) (get-wtf-site id))
              (if (and title url)
                  (let* ((mywtf (get-wtf-user (user-val 'id) id))
                         (reason (and mywtf (wtf-reason mywtf)))
                         (other (not (member reason `("lame" "duplicate" "bad link" "spam") :test #'string=))))
                    (htm
                     (:h1 "report site")
                     (:h2 (esc title))
                     (when (logged-in-p)
                       (htm
                        (:p (hbar "reason for karmic retribution"))
                        (when reason (htm (:span :class "error" "you have reported this link")))
                        (:form :class "nomargin" :id "repform" :onsubmit (makestr "wtf("id"); return false")
                               (:table
                                (:tr 
                                 (:td (:input :class "radio" :type "radio" 
                                              :value "lame" :name "reason"
                                              :checked (or (not reason) (string= reason "lame")) "lame"))
                                 (:td (:input :class "radio" :type "radio"
                                              :value "duplicate" :name "reason"
                                              :checked (string= reason "duplicate") "duplicate"))
                                 (:td (:input :class "radio" :type "radio"
                                              :value "bad link" :name "reason"
                                              :checked (string= reason "bad link") "bad link"))
                                 (:td (:input :class "radio" :type "radio"
                                              :value "spam" :name "reason"
                                              :checked (string= reason "spam") "spam"))
                                 (:td (:input :class "radio" :type "radio"
                                              :value "other" :name "reason"
                                              :checked (and reason other) :id "radother" "other"))
                                 (:td (:input :type "text" :name "desc"
                                              :value (and other reason) :onfocus "focusother()"))
                                 (:td (:button :class "btn" :type "submit" "save"))
                                 (:td (:button :class "btn" :onclick (makestr "unwtf("id"); return false") "remove"))
                                 (:td :class "error" :id (idstr "repstatus") " "))))))
                     (:p (hbar "previous reasons"))
                  (:table 
                   (loop for wtf in others do
                        (htm
                         (:tr
                          (:td :class "reptable little" (esc (print-date (wtf-date wtf))))
                          (:td :class "reptable" (user-link (user-name (wtf-user wtf))))
                          (:td :class "reptable" (esc (wtf-reason wtf)))))))))
              (htm (:span :class "error" "that site does not exist"))))))))

function wtf(id) {
    var status = document.getElementById("repstatus"+id);
    var form = document.getElementById("repform");
    var radother = document.getElementById("radother");
    if(radother.checked && form.desc.value == "") {
        status.innerHTML = "enter a reason";
        return false;
    }
    status.innerHTML = "saving...";
    

    new Ajax.Request('/aop', {parameters: "action=wtf&id="+id+"&"+Form.serialize(form),
                             onComplete:function(r){window.location.reload()}});
}

;;----------------------------- wtf site ------------------------------

(defun get-wtf-user (userid site)
  (car (select 'wtf :where [and [= userid [userid]]
                                [= site [article]]]
               :flatp t )))

(defun get-wtf-site (site)
  "Returns the number of dupes, the number of bads, and a list of
  the others."
   (select 'wtf :where [= [article] site] :order-by `(([date] desc)) :flatp t ))

(defun wtf (userid site reason)
  (and userid site reason
       (let ((wtf (or (get-wtf-user userid site)
                      (make-instance 'wtf :userid userid :articleid site))))
         (log-message* "WTF user: ~a site: ~a reason: ~a" userid site reason)
         (setf (wtf-reason wtf) (shorten-str reason 250)
               (wtf-date wtf) (get-time))
         (update-records-from-instance wtf))))

(defun remove-wtf (userid site)
  (when-bind (wtf (get-wtf-user userid site))
    (log-message* "UNWTF userid: ~a article: ~a" userid site)
    (delete-instance-records wtf)))

(defun wtf-and-mod (userid articleid reason ip)
  (when-valid (:userid userid :articleid articleid :ip ip)
    (if reason
        (progn
          (wtf userid articleid reason)
          (like-site userid articleid nil)
          (check-and-mod-article userid articleid ip -1)
          (check-and-mod-user userid articleid ip -1))
        (progn
          (remove-wtf userid articleid)
          (check-and-mod-article userid articleid ip 0)
          (check-and-mod-user userid articleid ip 0)))))


(define-callback wtf (id reason desc)
  (let ((id (sanitize id 'int)))
    (cond ((member reason '("lame" "duplicate" "bad link" "spam") :test #'string=)
           (wtf-and-mod (user-val 'id) id reason (session-remote-addr *session*)))
          ((and (string= reason "other") (> (length desc) 0))
           (wtf-and-mod (user-val 'id) id desc (session-remote-addr *session*))))))

(define-callback unwtf (id)
  (let ((id (sanitize id 'int)))
    (wtf-and-mod (user-val 'id) id nil (session-remote-addr *session*))))

function toggle(id, cl) {
    var toggle = document.getElementById("tog" + id);
    var dir;
    if (toggle.className == "star yellow") {
        if (cl || CLICKS[id]) {
            toggle.className = "star gray";
            setStarCookie(id, "g");
        }
        else {
            toggle.className = "star clear";
            setStarCookie(id, "c");
        }
        dir = 0;
    }
    else {
        toggle.className = "star yellow";
        setStarCookie(id, "y");
        dir = 1;
    }

    new Ajax.Request('/aop', {parameters: "action=mod&id="+id+"&dir="+dir});
}

