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

(defun rss-sites (rssurl siteurl name sort)
  (let ((sites (get-articles-cached 25 0 sort)))
    (with-html-output-to-string (*standard-output* nil)
      (format t "<?xml version='1.0' encoding='UTF-8'?>")
      (format t "<rdf:RDF xmlns:rdf='http://www.w3.org/1999/02/22-rdf-syntax-ns#' xmlns='http://purl.org/rss/1.0/'>")
      (:channel :|rdf:about| rssurl
                (:title (esc (conc "reddit.com: " name)))
                (:link (esc siteurl))
                (:description "what's new on the web")
                (:items
                 (format t "<rdf:Seq>")
                 (loop for article in sites do
                      (with-accessors ((title article-title)
                                       (url article-url)) article
                        (htm (:|rdf:li| :|rdf:resource| (escape-string url)))))
                 (format t "</rdf:Seq>")))
      (loop for article in sites do
           (with-accessors ((title article-title)
                            (url article-url)) article
             (htm
              (:item :|rdf:about| (escape-string url)
                     (:title (esc title))
                     (:link (esc url))))))
      (format t "</rdf:RDF>"))))
  
(defun rss-hot ()
  (setf (content-type) "text/xml")
  (cached ("rsshot" 900)
    (rss-sites "http://reddit.com/rss/hot" "http://reddit.com/" "hottest" :front)))

(defun rss-new ()
  (setf (content-type) "text/xml")
  (cached ("rssnew" 900)
    (rss-sites "http://reddit.com/rss/new" "http://reddit.com/new" "newest" :new)))

(defun rss-pop ()
  (setf (content-type) "text/xml")
  (cached ("rsspop" 900)
    (rss-sites "http://reddit.com/rss/pop" "http://reddit.com/pop" "top all-time" :pop)))
