(in-package :cl-user)
(defpackage gomoku-asd
  (:use :cl :asdf))
(in-package :gomoku-asd)

(defsystem gomoku
  :version "0.1"
  :author "Fei Peng"
  :license ""
  :depends-on (:clack
               :caveman2
               :envy
               :cl-ppcre

               ;; HTML Template
               :cl-emb

               ;; for CL-DBI
               :datafly
               :sxql

               ;;
               :cl-ppcre)
  :components ((:module "src"
                :components
                ((:file "main" :depends-on ("config" "view" "db"))
                 (:file "web" :depends-on ("view" "search"))
                 (:file "view" :depends-on ("config"))
                 (:file "db" :depends-on ("config"))
                 (:file "config")
                 (:file "search"))))
  :description ""
  :in-order-to ((test-op (load-op gomoku-test))))
