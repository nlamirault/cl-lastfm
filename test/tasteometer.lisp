;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          tasteometer.lisp
;;;; Purpose:       cl-lastfm unit tests for TasteOMeter module of LastFM API.
;;;; Programmer:    Nicolas Lamirault <nlamirault@gmail.com>
;;;;
;;;; This file, part of cl-lastfm, is Copyright (c) 2009 by Nicolas Lamirault
;;;;
;;;; cl-lastfm users are granted the rights to distribute and use this software
;;;; as governed by the terms of the MIT License :
;;;; http://www.opensource.org/licenses/mit-license.php
;;;;
;;;; *************************************************************************



(in-package :cl-lastfm-test)



(lift:addtest (cl-lastfm-test)
  test-tasteometer-compare-without-api-key
  (handler-case 
      (cl-lastfm::tasteometer-compare nil
                                      ""
                                      "user" "user" "joanofarctan" "mirandason")
    (cl-lastfm:lastfm-request-error (condition)
      (lift:ensure (equal (type-of condition)
                          'cl-lastfm:lastfm-request-error))
      (lift:ensure
       (string-equal "10"
                     (cl-lastfm:request-error-code-of condition))))))



(lift:addtest (cl-lastfm-test)
  test-tasteometer-compare-with-user
  (let ((response
         (cl-lastfm::tasteometer-compare
          "b25b959554ed76058ac220b7b2e0a026"
          "user" "user" "joanofarctan" "mirandason")))
    (lift:ensure (cl-ppcre:scan "<lfm status=\"ok\">" response))
    (lift:ensure (cl-ppcre:scan "<comparison>" response))
    (lift:ensure (cl-ppcre:scan "<score>" response))
    (lift:ensure (cl-ppcre:scan "<name>joanofarctan</name>" response))
    (lift:ensure (cl-ppcre:scan "<name>mirandason</name>" response))))
