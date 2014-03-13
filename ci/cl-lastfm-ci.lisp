;;
;; Continuous integration: launch unit tests
;;

(in-package :cl-user)

(load ".clenv/.quicklisp/setup.lisp")

(ql:quickload "cl-lastfm")
(ql:quickload "cl-lastfm-test")

(format t "API Key: ~A" (sb-posix:getenv "CL-LASTFM-API-KEY"))

(setq lisp-unit:*print-failures* t)
(setq lisp-unit:*print-errors* t)
(setq lisp-unit:*print-summary* t)
(lisp-unit:run-tests :all :cl-lastfm-test)
