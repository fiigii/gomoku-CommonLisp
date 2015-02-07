(in-package :cl-user)
(defpackage gomoku-test-asd
  (:use :cl :asdf))
(in-package :gomoku-test-asd)

(defsystem gomoku-test
  :author "Fei Peng"
  :license ""
  :depends-on (:gomoku
               :cl-test-more)
  :components ((:module "t"
                :components
                ((:file "gomoku"))))
  :perform (load-op :after (op c) (asdf:clear-system c)))
