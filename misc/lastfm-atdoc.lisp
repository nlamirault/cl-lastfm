;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;; Copyright (C) 2014  Nicolas Lamirault

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
;; THE SOFTWARE.

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
