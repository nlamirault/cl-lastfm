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
	       (:file "setup" :depends-on ("package"))
               ;; (:file "lastfm-tests" :depends-on ("specials"))
               (:file "albums" :depends-on ("setup"))
               ;; (:file "artists" :depends-on ("lastfm-tests"))
               (:file "geo" :depends-on ("setup"))
               (:file "users" :depends-on ("setup"))
	       (:file "events" :depends-on ("setup"))
	       (:file "library" :depends-on ("setup"))
               ;; (:file "tag" :depends-on ("lastfm-tests"))
               (:file "tracks" :depends-on ("setup"))
               ;; (:file "tasteometer" :depends-on ("lastfm-tests"))
               ;; (:file "unit-tests" :depends-on ("lastfm-tests"))))))
	       ))))
