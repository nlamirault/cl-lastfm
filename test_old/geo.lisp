;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          geo.lisp
;;;; Purpose:       cl-lastfm unit tests for Geo module of LastFM API.
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
;;
;; Geo
;;



;; Get Events


(lift:addtest (cl-lastfm-test)
  test-geo-get-events-without-api-key
  (handler-case 
      (cl-lastfm:geo-get-events nil :location "Bordeaux")
    (cl-lastfm:lastfm-request-error (condition)
      (lift:ensure (equal (type-of condition)
                          'cl-lastfm:lastfm-request-error))
      (lift:ensure
       (string-equal "10"
                     (cl-lastfm:request-error-code-of condition))))))


(lift:addtest (cl-lastfm-test)
  test-geo-get-events-with-location-name
  (let ((response
         (cl-lastfm:geo-get-events "b25b959554ed76058ac220b7b2e0a026"
                                   :location "Bordeaux")))
    (lift:ensure (cl-ppcre:scan "<lfm status=\"ok\">" response))
    (lift:ensure (cl-ppcre:scan "<events location=\"Bordeaux, France\""
                                response))
    (let ((sum 0))
      (cl-ppcre:do-matches (s e "<event>" response)
        (incf sum))
      (lift:ensure (> sum 0)))))


(lift:addtest (cl-lastfm-test)
  test-geo-get-events-with-lat-long
  (let ((response
         (cl-lastfm:geo-get-events "b25b959554ed76058ac220b7b2e0a026"
                                   :lat "44.8333333"
                                   :long "-0.5666667")))
    (lift:ensure (cl-ppcre:scan "<lfm status=\"ok\">" response))
    (lift:ensure (cl-ppcre:scan "<events location=" response))
    (let ((sum 0))
      (cl-ppcre:do-matches (s e "<event>" response)
        (incf sum))
      (lift:ensure (> sum 0)))))




;; Top artists


(lift:addtest (cl-lastfm-test)
  test-geo-top-artists-without-api-key
  (handler-case 
      (cl-lastfm:geo-top-artists nil "France")
    (cl-lastfm:lastfm-request-error (condition)
      (lift:ensure (equal (type-of condition)
                          'cl-lastfm:lastfm-request-error))
      (lift:ensure
       (string-equal "10"
                     (cl-lastfm:request-error-code-of condition))))))


(lift:addtest (cl-lastfm-test)
  test-geo-top-artists-with-country-name
  (let ((response
         (cl-lastfm:geo-top-artists "b25b959554ed76058ac220b7b2e0a026"
                                    "France")))
    (lift:ensure (cl-ppcre:scan "<lfm status=\"ok\">" response))
    (lift:ensure (cl-ppcre:scan "<topartists country=\"France\""
                                response))
    (let ((sum 0))
      (cl-ppcre:do-matches (s e "<artist " response)
        (incf sum))
      (lift:ensure (> sum 0)))))



;; Top tracks


(lift:addtest (cl-lastfm-test)
  test-geo-top-tracks-without-api-key
  (handler-case 
      (cl-lastfm:geo-top-tracks nil "France")
    (cl-lastfm:lastfm-request-error (condition)
      (lift:ensure (equal (type-of condition)
                          'cl-lastfm:lastfm-request-error))
      (lift:ensure
       (string-equal "10"
                     (cl-lastfm:request-error-code-of condition))))))


(lift:addtest (cl-lastfm-test)
  test-geo-top-tracks-with-country-name
  (let ((response
         (cl-lastfm:geo-top-tracks "b25b959554ed76058ac220b7b2e0a026"
                                   "France")))
    (lift:ensure (cl-ppcre:scan "<lfm status=\"ok\">" response))
    (lift:ensure (cl-ppcre:scan "<toptracks country=\"France\""
                                response))
    (let ((sum 0))
      (cl-ppcre:do-matches (s e "<track " response)
        (incf sum))
      (lift:ensure (> sum 0)))))


(lift:addtest (cl-lastfm-test)
  test-geo-top-tracks-with-country-and-region-names
  (let ((response
         (cl-lastfm:geo-top-tracks "b25b959554ed76058ac220b7b2e0a026"
                                   "France" "Bordeaux")))
    (lift:ensure (cl-ppcre:scan "<lfm status=\"ok\">" response))
    (lift:ensure (cl-ppcre:scan "<toptracks country=\"France\" metro=\"Bordeaux\""
                                response))
    (let ((sum 0))
      (cl-ppcre:do-matches (s e "<track " response)
        (incf sum))
      (lift:ensure (> sum 0)))))






