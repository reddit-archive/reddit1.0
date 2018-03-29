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

(defparameter *frame-height* "30px")

(defun reddit-frame (article)
  (with-html-output-to-string (*standard-output* nil :prologue "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Frameset//EN\" \"http://www.w3.org/TR/html4/frameset.dtd\">" :indent t)
    (:html
     (:head
      (:title (esc (article-title article))))
     (:frameset
      :framespacing 0 :rows (makestr *frame-height* ", 100%")
      (:frame :frameborder 0 :scrolling "no" :src (makestr "/toolbar?id=" (article-id article)))
      (:frame :frameborder 0 :src (article-url article))))))

(defun reddit-toolbar ()
  (with-parameters ((id "id"))
    (let* ((article (get-article (sanitize id 'int)))
           (modded (and (get-info (uid)) (user-liked (get-info (uid)) (article-id article))))
           (id (article-id article)))
      (with-html-output-to-string (*standard-output* nil :prologue t)
        (:head
         (:link :rel "stylesheet" :href "/static/framestyle.css" :type "text/css")
         (:script :src "/static/cookie.js" :language "javascript" :type "text/javascript" "")
         (:script :src "/static/psrs.js" :language "javascript" :type "text/javascript" "")
         (:script :language "javascript" (str (if (logged-in-p) "var logged = true" "var logged= false")))
         (:script :src "/static/mod.js" :language "javascript" :type "text/javascript" "")
         (:script :src "/static/frame.js" :language "javascript" :type "text/javascript" ""))
        (:body 
         (:form :name "log" :onsubmit "$(\"logbtn\").onclick(); return false"
                (:table
                 :style (makestr "height: " *frame-height* "; border-bottom: 1px solid black")
                 (:tr     
                  :id "killed" :style "display: none" :class "menu"
                  (:td :nowrap t
                   "after reloading, this frame will not be shown again. click "
                   (:a :href "javascript:unkill()" "here") " to undo.")
                  (:td :width "100%"))
                 (:tr
                  :id "main"
                  (:td 
                   (:a :target "_parent" :href "/" (:img :style "border: none" :src "/static/littlehead.png"
                                                             :alt "reddit.com" :title "reddit.com")))
                  (:td
                   (:div :id (idstr "up")
                         :class (if (eq modded :like) "arrow upmod" "arrow up")
                         :onclick (makestr "javascript:mod("id", 1)") " "))
                  (:td "like")
                  (:td :nowrap t
                       (:div :id (idstr "down")
                             :class (if (eq modded :dislike) "arrow downmod" "arrow down")
                             :onclick (makestr "javascript:mod("id", 0)")))
                  (:td "dislike")
                  (:td :id "left" :style "padding-left: 10px" :class "menu" :nowrap t
                       ;;(str "&nbsp;")
                       (:a :target "_parent" :href (makestr "/recommend?id=" id) "share")
                       (str "|")
                       (:a :target "_parent" :href "/lucky" "i'm feeling serendipitous"))
                  (:td :id "err" :style "text-align: right" :class "error" :width "100%")
                  (:td :id "middle" :nowrap t :style "display: none"
                       ;;username box
                       (:input :id "usrtxt" :type "text" :style "color: gray" :class "txt" :size 10
                               :value "username" :autocomplete "off" :onfocus "swapel(\"usrtxt\", \"usr\")")
                       (:input :id "usr" :type "text" :class "txt" :size 10)
                       ;;password box
                       (:input :id "passtxt" :type "text" :style "color: gray" :class "txt" :size 10
                               :value "password" :autocomplete "off" :onfocus "swapel(\"passtxt\", \"pass\")")
                       (:input :id "pass" :name "pass" :type "password" :class "txt" :size 10)
                       ;;verify password
                       (:input :id "vertxt" :type "text" :style "color: gray" :class "txt" :size 12
                               :value "verify password" :autocomplete "off" :onfocus "swapel(\"vertxt\", \"ver\")")
                       (:input :id "ver" :type "password" :class "txt" :size 12)
                       ;;remember me
                       (:input :id "rem" :name "rem" :type "checkbox")
                       (:label :id "remlbl" :for "rem" "keep me logged in")
                       ;;login register cancel buttons
                       (:button :id "logbtn" :class "btn" :type "submit" :onclick "login(); return false;" "login")
                       (:button :class "btn" :onclick "cancel(); return false" "cancel"))
                  (if (logged-in-p)
                      (htm
                       (:td :id "logmenu" :class "menu" :nowrap t 
                            (format t "~a (~a) |" (user-name (userobj)) (user-karma (userobj)))
                            (:a :href (conc "/user/" (user-name (userobj))) :target "_parent" "profile")
                            (str "|")
                            (:a :href "javascript:logout()" "logout")))
                      (htm
                       (:td :id "menu" :class "menu" :nowrap t
                            (:a :href "javascript:showlogin()" "login")
                            (str "|")
                            (:a :href "javascript:showreg()" "register"))))
                  (:td 
                   :id "buttons" :nowrap t
                   (:a :target "_parent" :href "/help/help.html"
                       (:img :style "border: none" :src "/static/help.png"
                             :alt "help" :title "help"))
                   (:a :target "_parent" :href (article-url article)
                       (:img :style "border: none" :src "/static/breakout.png"
                             :alt "open without frame" :title "open without frame"))
                   (:img :style "cursor: pointer" :src "/static/kill.png"
                         :alt "permanently close this frame" :title "permanently close this frame"
                         :onclick "kill()"))))))))))
