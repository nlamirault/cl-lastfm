;;
;; Continuous integration: launch unit tests
;;

(in-package :cl-user)

(load ".clenv/.quicklisp/setup.lisp")

(ql:quickload "cl-lastfm")
(ql:quickload "cl-lastfm-test")

(cl-lastfm-test:run-cl-lastfm-test)
