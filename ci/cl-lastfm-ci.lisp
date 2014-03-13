;;
;; Continuous integration: launch unit tests
;;

(in-package :cl-user)

(load ".clenv/.quicklisp/setup.lisp")

(ql:quickload "cl-lastfm")
(ql:quickload "cl-lastfm-test")

(lisp-unit:run-tests :all :cl-lastfm-test)
