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

(defparameter *min-front-page-pop* 2)
(defparameter *prob-threshold* .7)
(defparameter *hot-factor* 2000)

(destroy-processes "cached-hot")
(destroy-processes "cached-new")
(destroy-processes "cached-pop")

(defun get-articles (&optional (limit 25) (offset 0) (sort :front))
  (select 'article-with-sn
          :where  (if (eql sort :front) [> [pop] *min-front-page-pop*] t)
          :order-by (case sort
                      (:pop '(([pop] desc)))
                      (:new '(([articles_sn date] desc)))
                        (t `(;;(,(sql-operation 'function "recent_pop" [id]) desc)
                             (,[- [pop]
                                  [/ (sql-operation 'function "seconds" [date]) *hot-factor*]] desc))))
          :offset offset
          :limit limit
          :flatp t
          :caching nil))

(defparameter *cached-hot* (make-instance 'ac :name "cached-hot" :period 30
                                          :fn #'(lambda () (with-web-db (get-articles 500 0 :front)))))
(defparameter *cached-new* (make-instance 'ac :name "cached-new" :period 30
                                          :fn #'(lambda () (with-web-db (get-articles 500 0 :new)))))
(defparameter *cached-pop* (make-instance 'ac :name "cached-pop" :period 30
                                          :fn #'(lambda () (with-web-db (get-articles 500 0 :pop)))))

;;(defparameter *cached-hot* nil)
;;(defparameter *cached-new* nil)
;;(defparameter *cached-pop* nil)

(defun get-articles-cached (&optional (limit 25) (offset 0) (sort :front))
  (let ((val (case sort
                    (:front (and *cached-hot* (ac-val *cached-hot*)))
                    (:new (and *cached-new* (ac-val *cached-new*)))
                    (:pop (and *cached-pop* (ac-val *cached-pop*))))))
    (if (> (+ offset limit) (length val))
        (get-articles limit offset sort)
        (subseq val offset (+ offset limit)))))
  
(defun profile-sites (userid limit offset display)
  "display can be :saved :hidden :submitted :promoted :demoted"
  (select 'article-with-sn
          :where (case display
                   (:saved [and [= userid [saved_sites userid]]
                                [= [articles_sn id] [saved_sites article]]])
                   (:hidden [and [= userid [closed_sites userid]]
                                 [= [articles_sn id] [closed_sites article]]])
                   (:submitted [= [submitter] userid])
                   (:promoted [and [= [like_site userid] userid]
                                   [= [like_site article] [articles_sn id]]
                                   [= [like_site liked] [true]]])
                   (:demoted [and [= [like_site userid] userid]
                                  [= [like_site article] [articles_sn id]]
                                  [= [like_site liked] [false]]]))
          ;;:group-by (sql-expression :string "articles.id, title, url, pop, screenname, articles.date")
          :order-by '(([articles_sn date] desc))
          :offset offset
          :limit limit
          :flatp t))

(defun site-tl (articleid)
  "Returns the title and link for a particlular site."
   (car (select [title] [url] :from [articles]
          :where [= [id] articleid]
          :flatp t
          )))
               
;;close sites
(defun unclose-site-sql (userid articleid)
  (unless (or (null userid)
              (null articleid))
    (delete-records :from [closed_sites] :where [and [= userid [userid]]
                                                     [= articleid [article]]] )))

(defun close-site-sql (userid articleid)
  (ignore-errors
    (unless (or (null userid)
                (null articleid))
      (insert-records :into [closed_sites] :values (list userid articleid)))))

(defun site-closed-p-sql (userid articleid)
  (and userid articleid
       (car (select [article] :from [closed_sites]
                    :where [and [= userid [userid]]
                                [= articleid [article]]]
                    :flatp t
                    ))))

(defun update-site-url (articleid url)
  (ignore-errors
    (update-records [articles] :attributes '(url) :values (list url) :where [= [id] articleid] )))

(defun update-nytimes-url (articleid url)
  (when (and (nytimes-link-p url)
             (not (good-nytimes-p url)))
    (let ((newurl (good-nytimes url)))
      (when newurl
        (update-site-url articleid newurl)))))

;;similar urls
(defun similar-urls (url)
  (select [id] [url] :from [articles] :where [like [url] (format nil "%~a%" url)] ))

(defun article-id-from-url (url)
  (when (> (length url) 0)
    (let ((url (base-url url)))
      (some #'(lambda (site)
                (when (string= (base-url (second site)) url)
                  (first site)))
            (similar-urls url)))))

;;saved sites
(defun save-site (userid articleid)
  (unless (or (null userid)
              (null articleid))
    (ignore-errors
      (insert-records :into [saved_sites] :values (list userid articleid)))))

(defun saved-sites (userid)
  (select [articles id] [title] [url] :from '([articles] [saved_sites])
          :where [and [= [userid] userid]
                      [= [articles id] [article]]] ))

(defun unsave-site (userid articleid)
  (ignore-errors
    (delete-records :from [saved_sites] :where [and [= [userid] userid]
                                                    [= [article] articleid]] )))

;;check-url
(defun check-url (url)
  "Returns the title for this url."
  (let* ((url (add-http url))
         (article (get-article-sn (article-id-from-url url))))
    (or (and article (article-title article)) (website-title url))))

(defun display-site-p (uinfo articleid)
  "Given an article-info and the option parameters, decide
whether the site should be display."
  (with-slots (promoted demoted numsites) (user-options uinfo)
    (and (or promoted (not (eq (user-liked uinfo articleid) :like)))
         (or demoted (not (eq (user-liked uinfo articleid) :dislike)))
         (not (user-closed uinfo articleid)))))
  
  
(defun filter-sites (userinfo articles)
  (remove-if-not #'(lambda (article) (display-site-p userinfo (article-id article))) articles))


(defun get-sites-user (userid limit offset sort)
  "Gets the next limit sites starting from offset that the user
  don't find offensive."
  (let ((userinfo (get-info userid)))
    (do* ((sites nil (append sites cursites))
          (offset offset (+ offset limit))
          (usites (get-articles-cached limit offset sort)
                  (get-articles-cached limit offset sort))
          (cursites (if userinfo (filter-sites userinfo usites) usites)
                    (if userinfo (filter-sites userinfo usites) usites)))
         ((or (null usites)
              (>= (+ (length cursites) (length sites)) limit))
          (if (and (null sites) (null cursites))
              nil
              (let ((final (subseq (append sites cursites) 0 (min limit (+ (or (length sites) 0)
                                                                           (or (length cursites) 0))))))
                (values final
                        (+ (or (position (car (last final)) usites) 0) offset 1))))))))

(defun get-sites-profile (userid profid limit offset display)
  (let ((sites (profile-sites profid limit offset display)))
    (values sites (+ offset (length sites)))))

(defun get-search-sites (userid query limit offset)
  (let ((sites (search-sites query limit offset)))
    (values sites (+ offset (length sites)))))
