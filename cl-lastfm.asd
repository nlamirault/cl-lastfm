;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          cl-lastfm.asd
;;;; Purpose:       ASDF definition for cl-lastfm
;;;; Programmer:    Nicolas Lamirault <nicolas.lamirault@gmail.com>
;;;;
;;;; This file, part of cl-lastfm, is Copyright (c) 2009 by Nicolas Lamirault
;;;;
;;;; cl-lastfm users are granted the rights to distribute and use this software
;;;; as governed by the terms of the MIT License :
;;;; http://www.opensource.org/licenses/mit-license.php
;;;;
;;;; *************************************************************************


(in-package :asdf)


(defsystem cl-lastfm
    :name "cl-lastfm"
    :author "Nicolas Lamirault <nicolas.lamirault@gmail.com>"
    :maintainer "Nicolas Lamirault <nicolas.lamirault@gmail.com>"
    :version "0.1"
    :licence "MIT License"
    :description "Common Lisp wrapper for the Last.fm web service."
    :depends-on (:drakma :cxml-stp :url-rewrite :trivial-utf-8)
    :components
    ((:module :src
              :components
              ((:file "package")
               (:file "conditions" :depends-on ("package"))
               (:file "specials" :depends-on ("package"))
               (:file "tools" :depends-on ("specials" "conditions"))
               (:file "lastfm" :depends-on ("tools"))))))

