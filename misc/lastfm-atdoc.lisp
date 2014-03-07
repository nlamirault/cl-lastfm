;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          lastfm-atdoc.lisp
;;;; Purpose:       cl-lastfm documentation tool
;;;; Programmer:    Nicolas Lamirault <nlamirault@gmail.com>
;;;;
;;;; This file, part of cl-lastfm, is Copyright (c) 2009 by Nicolas Lamirault
;;;;
;;;; cl-lastfm users are granted the rights to distribute and use this software
;;;; as governed by the terms of the MIT License :
;;;; http://www.opensource.org/licenses/mit-license.php
;;;;
;;;; *************************************************************************

;;(in-package :cl-user)

;; (load ".clenv/.quicklisp/setup.lisp")

;; (ql:quickload "cl-lastfm")
;; (ql:quickload "atdoc")

(defun make-lastfm-doc ()
  (let* ((path (namestring
		(asdf:component-relative-pathname (asdf:find-system :cl-lastfm))))
	 (dir (concatenate 'string path "/www/api/")))
    (ensure-directories-exist dir)
    (atdoc:generate-html-documentation '(:cl-lastfm)
				       dir
				       :index-title "cl-lastfm API reference"
				       :heading "LastFM for Common Lisp"
				       ;;:css "orange-sans.css"
				       :single-page-p t
				       :include-internal-symbols-p nil)
    (atdoc:generate-latex-documentation '(:cl-lastfm)
					dir
					:title "cl-lastfm API reference")
    (atdoc:generate-info-documentation '(:cl-lastfm)
				       dir
				       :name "cl-lastfm"
				       :title "cl-lastfm API referenc")))
