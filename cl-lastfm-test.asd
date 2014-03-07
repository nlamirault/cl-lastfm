;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          cl-lastfm-test.asd
;;;; Purpose:       ASDF definition for cl-lastfm-test
;;;; Programmer:    Nicolas Lamirault <nlamirault@gmail.com>
;;;;
;;;; This file, part of cl-lastfm, is Copyright (c) 2009 by Nicolas Lamirault
;;;;
;;;; cl-lastfm users are granted the rights to distribute and use this software
;;;; as governed by the terms of the MIT License :
;;;; http://www.opensource.org/licenses/mit-license.php
;;;;
;;;; *************************************************************************


(defsystem #:cl-lastfm-test
    :name "cl-lastfm-test"
    :author "Nicolas Lamirault <nlamirault@gmail.com>"
    :maintainer "Nicolas Lamirault <nlamirault@gmail.com>"
    :version "0.1"
    :licence "MIT License"
    :description "Unit tests for cl-lastfm."
    :depends-on (#:cl-lastfm #:lisp-unit)
    :serial t
    :components
    ((:module :test
              :components
              ((:file "package")
               ;; (:file "specials" :depends-on ("package"))
               ;; (:file "lastfm-tests" :depends-on ("specials"))
               ;; (:file "albums" :depends-on ("lastfm-tests"))
               ;; (:file "artists" :depends-on ("lastfm-tests"))
               ;; (:file "geo" :depends-on ("lastfm-tests"))
               (:file "users" :depends-on ("package"))
               ;; (:file "events" :depends-on ("lastfm-tests"))
               ;; (:file "library" :depends-on ("lastfm-tests"))
               ;; (:file "tag" :depends-on ("lastfm-tests"))
               ;; (:file "track" :depends-on ("lastfm-tests"))
               ;; (:file "tasteometer" :depends-on ("lastfm-tests"))
               ;; (:file "unit-tests" :depends-on ("lastfm-tests"))))))
	       ))))
