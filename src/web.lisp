(in-package :cl-user)
(defpackage gomoku.web
  (:use :cl
        :caveman2
        :gomoku.config
        :gomoku.view
        :gomoku.db
        :datafly
        :sxql
        :gomoku.search)
  (:export :*web*))
(in-package :gomoku.web)

;;
;; Application

(defclass <web> (<app>) ())
(defvar *web* (make-instance '<web>))
(clear-routing-rules *web*)

;;
;; Routing rules

(defroute "/" ()
  (with-layout (:title "Welcome to Caveman2")
    (restart_board)
    (render #P"index.tmpl")))


(defroute ("/user.json" :method :POST) (&key _parsed)
  (let ((postion (nextstep _parsed)))
    ;; person => (:|name| "Eitaro Fukamachi" :|email| "e.arrows@gmail.com")
    (render-json postion)))

(defroute "/init.json" (&key _parsed)
  (restart_board)
  (render-json '()))

;;
;; Error pages

(defmethod on-exception ((app <web>) (code (eql 404)))
  (declare (ignore app))
  (merge-pathnames #P"_errors/404.html"
                   *template-directory*))
