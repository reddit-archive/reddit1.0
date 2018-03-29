;;;; Silly emacs, this is -*- Lisp -*- (or thereabouts)

(in-package #:cl-user)

(defpackage #:reddit-system
  (:use #:asdf #:cl))

(in-package #:reddit-system)

(defsystem reddit
    :depends-on (:tbnl
                 :cl-ppcre
                 :trivial-http
                 :cl-who
                 :clsql
                 :clsql-postgresql
                 :cl-smtp
                 :ironclad)
    :components ((:file "packages")
                 (:file "cookiehash" :depends-on ("packages" "data"))
                 (:file "recommend" :depends-on ("packages" "user-info"))
                 (:file "frame" :depends-on ("packages" "web"))
                 (:file "autocompute" :depends-on ("packages"))
                 (:file "user-info" :depends-on ("data" "packages"))
                 (:file "web" :depends-on ("packages" "mail" "recommend" "data" "util" "mail" "rss" "memcached" "sites" "view-defs" "user-info" "cookiehash"))
                 (:file "data" :depends-on ("packages" "view-defs" "util"))
                 (:file "view-defs" :depends-on ("packages"))
                 (:file "mail" :depends-on ("packages"))
                 (:file "util" :depends-on ("packages"))
                 (:file "search" :depends-on ("packages"))
                 ;;(:file "options" :depends-on ("packages" "data"))
                 (:file "memcached" :depends-on ("packages" "crc"))
                 (:file "crc" :depends-on ("packages"))
                 (:file "rss" :depends-on ("memcached" "packages" "sites"))
                 (:file "sites" :depends-on ("packages" "data" "util" "search" "autocompute" "user-info"))
                 (:file "mail" :depends-on ("packages" "data"))
                 (:file "user-panel" :depends-on ("data" "packages" "web" "sites"))))
                 
