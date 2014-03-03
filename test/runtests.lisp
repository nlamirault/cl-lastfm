;;;; -*- Mode: LISP -*-

;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          runtests.lisp
;;;; Purpose:       Climon unit tests.
;;;; Programmer:    Nicolas Lamirault <nicolas.lamirault@gmail.com>
;;;;
;;;; *************************************************************************


(in-package :cl-user)

(load "./.quicklisp/setup.lisp")

(ql:quickload "cl-lastfm")
(ql:quickload "cl-lastfm-test")

(defun run-cl-lastfm-test ()
  "Run the CL-LASTFM unit tests."
   (let ((cl-ppcre:*regex-char-code-limit* 256)
	 (cl-ppcre:*use-bmh-matchers* nil)
	 (config-file (concatenate 'string
				   cl-lastfm-test:*cl-lastfm-path*
				   "etc/lift-standard.config")))
     (lift:run-tests :config config-file)))


(run-cl-lastfm-test)
