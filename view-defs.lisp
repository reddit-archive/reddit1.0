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

;;user
(def-view-class user ()
  ((id
    :db-kind :key
    :type integer
    :reader user-id)
   (screenname
    :type string
    :reader user-name)
   (email
    :type string
    :accessor user-emai)
   (karma
    :type integer
    :reader user-karma)
   (signupdate
    :type wall-time
    :reader user-date)
   (ip
    :type string
    :reader user-ip))
  (:base-table users))

(defmethod user-karma ((u user))
  (or (slot-value u 'karma) 0))

;;article
(def-view-class article ()
  ((id
    :db-kind :key
    :type integer
    :initarg :id
    :accessor article-id)
   (url
    :type string
    :accessor article-url
    :initarg :url)
   (title
    :type string
    :accessor article-title
    :initarg :title)
   (date
    :type wall-time
    :accessor article-date
    :initarg :date)
   (submitterid
    :column submitter
    :type integer
    :accessor article-submitterid
    :initarg :submitterid)
   (submitter
    :db-kind :join
    :db-info (:join-class user :home-key submitterid :foreign-key id :set nil)
    :reader article-submitter)
   (pop
    :type integer
    :reader article-pop))
  (:base-table articles))

(def-view-class article-with-sn (article)
  ((screenname
    :reader article-sn
    :type string))
  (:base-table articles_sn))

;;wtf
(def-view-class wtf ()
  ((userid
    :db-kind :key
    :type integer
    :initarg :userid
    :accessor wtf-userid)
   (user
    :db-kind :join
    :db-info (:join-class user :home-key userid :foreign-key id :set nil)
    :reader wtf-user)
   (article
    :db-kind :key
    :type integer
    :initarg :articleid
    :accessor wtf-articleid)
   (reason
    :type (string 250)
    :initarg :reason
    :accessor wtf-reason)
   (date
    :type wall-time
    :initarg :date
    :accessor wtf-date)))

;;click
(def-view-class click ()
  ((userid
    :type integer
    :initarg :userid
    :accessor click-userid)
   (article
    :type integer
    :initarg :articleid
    :accessor click-articleid)
   (date
    :type wall-time
    :initarg :date
    :initform (get-time)
    :accessor click-date)
   (ip
    :type string
    :initarg :ip
    :accessor click-ip))
  (:base-table clicks))

;;like_site
(def-view-class like ()
  ((userid
    :db-kind :key
    :type integer
    :initarg :userid
    :accessor like-userid)
   (article
    :db-kind :key
    :type integer
    :initarg :articleid
    :accessor like-articleid)
   (date
    :type wall-time
    :initarg :date
    :accessor like-date)
  (liked
   :type boolean
   :initarg :like
   :accessor like-like))
  (:base-table like_site))

;;mod_user
(def-view-class moduser ()
  ((userid
    :db-kind :key
    :type integer
    :initarg :userid
    :accessor moduser-userid)
   (article
    :db-kind :key
    :type integer
    :initarg :articleid
    :accessor moduser-articleid)
   (target
    :db-kind :key
    :type integer
    :initarg :targetid
    :accessor moduser-targetid)
   (date
    :type wall-time
    :initarg :date
    :accessor moduser-date)
   (ip
    :type string
    :initarg :ip
    :accessor moduser-ip)
   (amount
    :type integer
    :initarg :amount
    :accessor moduser-amount))
  (:base-table mod_user))

;;mod_article
(def-view-class modarticle ()
  ((userid
    :db-kind :key
    :type integer
    :initarg :userid
    :accessor modarticle-userid)
   (article
    :db-kind :key
    :type integer
    :initarg :articleid
    :accessor modarticle-articleid)
   (date
    :type wall-time
    :initarg :date
    :accessor modarticle-date)
   (ip
    :type string
    :initarg :ip
    :accessor modarticle-ip)
   (amount
    :type integer
    :initarg :amount
    :accessor modarticle-amount))
  (:base-table mod_article))

;;neuter
(def-view-class neuter ()
  ((userid
    :type integer
    :initarg :userid
    :accessor neuter-userid)
   (ip
    :type :string
    :initarg :ip
    :accessor neuter-ip))
  (:base-table neuter))

;;options
(def-view-class options ()
  ((userid
    :db-kind :key
    :type integer
    :initarg :userid
    :accessor options-userid)
   (numsites
    :type integer
    :initarg :numsites
    :accessor options-numsites)
   (promoted
    :type boolean
    :initarg :promoted
    :accessor options-promoted)
   (demoted
    :type boolean
    :initarg :demoted
    :accessor options-demoted)
   (visible
    :type boolean
    :initarg :visible
    :accessor options-visible)
   (frame
    :type boolean
    :initarg :frame
    :accessor options-frame))
  (:base-table options))

;;alias
(def-view-class alias ()
  ((userid
    :db-kind :key
    :type integer
    :initarg :userid
    :accessor alias-userid)
   (name
    :db-kind :key
    :type string
    :initarg :name
    :accessor alias-name)
   (val
    :type string
    :initarg :val
    :accessor alias-val))
  (:base-table alias))
