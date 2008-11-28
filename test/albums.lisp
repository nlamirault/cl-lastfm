;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          definitions.lisp
;;;; Purpose:       cl-lastfm Unit Tests suite.
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Albums



;; Albums : Get Infos


(lift:addtest (cl-lastfm-test)
  test-album-get-infos-without-api-key
  (handler-case 
      (cl-lastfm::album-get-info nil)
    (cl-lastfm::lastfm-request-error (condition)
      (lift:ensure (equal (type-of condition)
                          'cl-lastfm:lastfm-request-error))
      (lift:ensure
       (string-equal "10"
                     (cl-lastfm:request-error-code-of condition))))))


(lift:addtest (cl-lastfm-test)
  test-album-get-infos-with-artist
  (handler-case 
      (cl-lastfm::album-get-info "b25b959554ed76058ac220b7b2e0a026"
                                 :artist-name "cher")
    (cl-lastfm::lastfm-request-error (condition)
      (lift:ensure (equal (type-of condition)
                          'cl-lastfm::lastfm-request-error))
      (lift:ensure
       (string-equal "6"
                     (cl-lastfm:request-error-code-of condition))))))


(lift:addtest (cl-lastfm-test)
  test-album-get-infos-with-album
  (handler-case 
      (cl-lastfm::album-get-info "b25b959554ed76058ac220b7b2e0a026"
                                 :album-name "Believe")
    (cl-lastfm::lastfm-request-error (condition)
      (lift:ensure (equal (type-of condition)
                          'cl-lastfm::lastfm-request-error))
      (lift:ensure
       (string-equal "6"
                     (cl-lastfm:request-error-code-of condition))))))


(lift:addtest (cl-lastfm-test)
  test-album-get-infos-with-mbid
  (let ((response
         (cl-lastfm::album-get-info "b25b959554ed76058ac220b7b2e0a026"
                                    :mbid "61bf0388-b8a9-48f4-81d1-7eb02706dfb0")))
    (lift:ensure (cl-ppcre:scan "<lfm status=\"ok\">" response))))


(lift:addtest (cl-lastfm-test)
  test-album-get-infos-with-artist-and-album
  (let ((response
         (cl-lastfm::album-get-info "b25b959554ed76058ac220b7b2e0a026"
                                    :artist-name "cher"
                                    :album-name "Believe")))
    (lift:ensure (cl-ppcre:scan "<lfm status=\"ok\">" response))))


(lift:addtest (cl-lastfm-test)
  test-album-get-infos-with-mbid-and-lang
  (let ((response
         (cl-lastfm::album-get-info "b25b959554ed76058ac220b7b2e0a026"
                                    :mbid "61bf0388-b8a9-48f4-81d1-7eb02706dfb0"
                                    :lang "en")))
    (lift:ensure (cl-ppcre:scan "<lfm status=\"ok\">" response))))




;; Albums : Search 

(lift:addtest (cl-lastfm-test)
  test-album-search
  (let ((response
         (cl-lastfm::album-search "b25b959554ed76058ac220b7b2e0a026"
                                  "Hello" 
                                  :limit 10)))
    (lift:ensure (cl-ppcre:scan "<lfm status=\"ok\">" response))
    (let ((sum 0))
      (cl-ppcre:do-matches (s e "<album>" response)
        (incf sum))
      (lift:ensure (= 10 sum)))))



