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


(require :asdf)

(asdf:oos 'asdf:load-op :atdoc)
(asdf:oos 'asdf:load-op :cl-lastfm)
     
(atdoc:generate-documentation '(:cl-lastfm)
                              "/tmp/cl-lastfm-atdoc/"
                              :index-title "cl-lastfm API reference"
                              :heading "LastFM for Common Lisp"
                              :css "orange-sans.css"
                              :single-page-p t)

