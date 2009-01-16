;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          geo.lisp
;;;; Purpose:       cl-lastfm unit tests for Geo module of LastFM API.
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Users
;;



;; Get Events


(lift:addtest (cl-lastfm-test)
  test-user-get-events-without-api-key
  (handler-case 
      (cl-lastfm:user-get-events nil "rj")
    (cl-lastfm:lastfm-request-error (condition)
      (lift:ensure (equal (type-of condition)
                          'cl-lastfm:lastfm-request-error))
      (lift:ensure
       (string-equal "10"
                     (cl-lastfm:request-error-code-of condition))))))



(lift:addtest (cl-lastfm-test)
  test-user-get-events-with-name
  (let ((response
         (cl-lastfm:user-get-events "b25b959554ed76058ac220b7b2e0a026"
                                    "mokele")))
    (lift:ensure (cl-ppcre:scan "<lfm status=\"ok\">" response))
    (lift:ensure (cl-ppcre:scan "<events user=" response))
    (let ((sum 0))
      (cl-ppcre:do-matches (s e "<user>" response)
        (incf sum))
      (lift:ensure (>= sum 0)))))



;; Get Friends

(lift:addtest (cl-lastfm-test)
  test-user-get-friends-without-api-key
  (handler-case 
      (cl-lastfm:user-get-friends nil "rj")
    (cl-lastfm:lastfm-request-error (condition)
      (lift:ensure (equal (type-of condition)
                          'cl-lastfm:lastfm-request-error))
      (lift:ensure
       (string-equal "10"
                     (cl-lastfm:request-error-code-of condition))))))


(lift:addtest (cl-lastfm-test)
  test-user-get-friends-with-name
  (let ((response
         (cl-lastfm:user-get-friends "b25b959554ed76058ac220b7b2e0a026"
                                    "rj")))
    (lift:ensure (cl-ppcre:scan "<lfm status=\"ok\">" response))
    (lift:ensure (cl-ppcre:scan "<friends for="
                                response))))


(lift:addtest (cl-lastfm-test)
  test-user-get-friends-with-name-and-limit
  (let ((response
         (cl-lastfm:user-get-friends "b25b959554ed76058ac220b7b2e0a026"
                                     "rj" :limit 5)))
    (lift:ensure (cl-ppcre:scan "<lfm status=\"ok\">" response))
    (lift:ensure (cl-ppcre:scan "<friends for="
                                response))))


(lift:addtest (cl-lastfm-test)
  test-user-get-friends-with-name-and-recent-tracks
  (let ((response
         (cl-lastfm:user-get-friends "b25b959554ed76058ac220b7b2e0a026"
                                     "rj" :recenttracks "true")))
    (lift:ensure (cl-ppcre:scan "<lfm status=\"ok\">" response))
    (lift:ensure (cl-ppcre:scan "<friends for="
                                response))
    (lift:ensure (cl-ppcre:scan "<recenttrack>"
                                response))
    (lift:ensure (cl-ppcre:scan "</recenttrack>"
                                response))))


;; Loved tracks

(lift:addtest (cl-lastfm-test)
  test-user-get-loved-tracks-without-api-key
  (handler-case 
      (cl-lastfm:user-get-loved-tracks nil "rj")
    (cl-lastfm:lastfm-request-error (condition)
      (lift:ensure (equal (type-of condition)
                          'cl-lastfm:lastfm-request-error))
      (lift:ensure
       (string-equal "10"
                     (cl-lastfm:request-error-code-of condition))))))


(lift:addtest (cl-lastfm-test)
  test-user-get-loved-tracks-with-name
  (let ((response
         (cl-lastfm:user-get-loved-tracks "b25b959554ed76058ac220b7b2e0a026" "rj")))
    (lift:ensure (cl-ppcre:scan "<lfm status=\"ok\">" response))
    (lift:ensure (cl-ppcre:scan "<lovedtracks user="
                                response))
    (lift:ensure (cl-ppcre:scan "<track>"
                                response))))


;; Neighbours


(lift:addtest (cl-lastfm-test)
  test-user-get-neighbours-without-api-key
  (handler-case 
      (cl-lastfm:user-get-neighbours nil "rj")
    (cl-lastfm:lastfm-request-error (condition)
      (lift:ensure (equal (type-of condition)
                          'cl-lastfm:lastfm-request-error))
      (lift:ensure
       (string-equal "10"
                     (cl-lastfm:request-error-code-of condition))))))


(lift:addtest (cl-lastfm-test)
  test-user-get-neighbours-with-name-and-limit
  (let ((response
         (cl-lastfm:user-get-neighbours "b25b959554ed76058ac220b7b2e0a026" "rj"
                                        :limit 5)))
    (lift:ensure (cl-ppcre:scan "<lfm status=\"ok\">" response))
    (lift:ensure (cl-ppcre:scan "<neighbours user="
                                response))
    (let ((sum 0))
      (cl-ppcre:do-matches (s e "<user>" response)
        (incf sum))
      (lift:ensure (= sum 5)))))
