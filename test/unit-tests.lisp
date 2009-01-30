;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          unit-tests.lisp
;;;; Purpose:       cl-lastfm unit tests.
;;;; Programmer:    Nicolas Lamirault <nlamirault@gmail.com>
;;;;
;;;; This file, part of cl-lastfm, is Copyright (c) 2008 by Nicolas Lamirault
;;;;
;;;; cl-lastfm users are granted the rights to distribute and use this software
;;;; as governed by the terms of the MIT License :
;;;; http://www.opensource.org/licenses/mit-license.php
;;;;
;;;; *************************************************************************



(in-package :cl-lastfm-test)


(defun run-cl-lastfm-test ()
  "Run the CL-LASTFM unit tests."
;;;   (setq cl-ppcre:*regex-char-code-limit* 256
;;;         cl-ppcre:*use-bmh-matchers* nil)
  (let ((cl-ppcre:*regex-char-code-limit* 256)
        (cl-ppcre:*use-bmh-matchers* nil)
        (config-file (concatenate 'string
                                  *cl-lastfm-path*
                                  "etc/lift-standard.config")))
    (lift:run-tests :config config-file)))




