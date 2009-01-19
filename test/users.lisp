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
                                    "rj")))
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
    (lift:ensure (cl-ppcre:scan "<neighbours user=" response))
    (let ((sum 0))
      (cl-ppcre:do-matches (s e "<user>" response)
        (incf sum))
      (lift:ensure (= sum 5)))))



;; Top albums


(lift:addtest (cl-lastfm-test)
  test-user-get-top-albums-without-api-key
  (handler-case 
      (cl-lastfm:user-get-top-albums nil "rj")
    (cl-lastfm:lastfm-request-error (condition)
      (lift:ensure (equal (type-of condition)
                          'cl-lastfm:lastfm-request-error))
      (lift:ensure
       (string-equal "10"
                     (cl-lastfm:request-error-code-of condition))))))


(lift:addtest (cl-lastfm-test)
  test-user-get-top-albums
  (let ((response
         (cl-lastfm:user-get-top-albums "b25b959554ed76058ac220b7b2e0a026" "rj")))
    (lift:ensure (cl-ppcre:scan "<lfm status=\"ok\">" response))
    (lift:ensure (cl-ppcre:scan "<topalbums user=" response))
    (let ((sum 0))
      (cl-ppcre:do-matches (s e "<album  rank=" response)
        (incf sum))
      (lift:ensure (> sum 0)))))




;; Top artists


(lift:addtest (cl-lastfm-test)
  test-user-get-top-artists-without-api-key
  (handler-case 
      (cl-lastfm:user-get-top-artists nil "rj")
    (cl-lastfm:lastfm-request-error (condition)
      (lift:ensure (equal (type-of condition)
                          'cl-lastfm:lastfm-request-error))
      (lift:ensure
       (string-equal "10"
                     (cl-lastfm:request-error-code-of condition))))))


(lift:addtest (cl-lastfm-test)
  test-user-get-top-artists
  (let ((response 
         (cl-lastfm:user-get-top-artists "b25b959554ed76058ac220b7b2e0a026" "rj")))
    (lift:ensure (cl-ppcre:scan "<lfm status=\"ok\">" response))
    (lift:ensure (cl-ppcre:scan "<topartists user=" response))
    (let ((sum 0))
      (cl-ppcre:do-matches (s e "<artist  rank=" response)
        (incf sum))
      (lift:ensure (> sum 0)))))



;; Top tags


(lift:addtest (cl-lastfm-test)
  test-user-get-top-tags-without-api-key
  (handler-case 
      (cl-lastfm:user-get-top-tags nil "rj")
    (cl-lastfm:lastfm-request-error (condition)
      (lift:ensure (equal (type-of condition)
                          'cl-lastfm:lastfm-request-error))
      (lift:ensure
       (string-equal "10"
                     (cl-lastfm:request-error-code-of condition))))))


(lift:addtest (cl-lastfm-test)
  test-user-get-top-tags
  (let ((response 
         (cl-lastfm:user-get-top-tags "b25b959554ed76058ac220b7b2e0a026" "rj")))
    (lift:ensure (cl-ppcre:scan "<lfm status=\"ok\">" response))
    (lift:ensure (cl-ppcre:scan "<toptags user=" response))
    (let ((sum 0))
      (cl-ppcre:do-matches (s e "<tag>" response)
        (incf sum))
      (lift:ensure (> sum 0)))))



;; Top tracks


(lift:addtest (cl-lastfm-test)
  test-user-get-top-tracks-without-api-key
  (handler-case 
      (cl-lastfm:user-get-top-tracks nil "rj")
    (cl-lastfm:lastfm-request-error (condition)
      (lift:ensure (equal (type-of condition)
                          'cl-lastfm:lastfm-request-error))
      (lift:ensure
       (string-equal "10"
                     (cl-lastfm:request-error-code-of condition))))))


(lift:addtest (cl-lastfm-test)
  test-user-get-top-tracks
  (let ((response
         (cl-lastfm:user-get-top-tracks "b25b959554ed76058ac220b7b2e0a026" "rj")))
    (lift:ensure (cl-ppcre:scan "<lfm status=\"ok\">" response))
    (lift:ensure (cl-ppcre:scan "<toptracks user=" response))
    (let ((sum 0))
      (cl-ppcre:do-matches (s e "<track rank=" response)
        (incf sum))
      (lift:ensure (> sum 0)))))



;; Recent tracks


(lift:addtest (cl-lastfm-test)
  test-user-get-recent-tracks-without-api-key
  (handler-case 
      (cl-lastfm:user-get-recent-tracks nil "rj")
    (cl-lastfm:lastfm-request-error (condition)
      (lift:ensure (equal (type-of condition)
                          'cl-lastfm:lastfm-request-error))
      (lift:ensure
       (string-equal "10"
                     (cl-lastfm:request-error-code-of condition))))))


(lift:addtest (cl-lastfm-test)
  test-user-get-recent-tracks
  (let ((response
         (cl-lastfm:user-get-recent-tracks "b25b959554ed76058ac220b7b2e0a026" "rj"
                                           :limit 5)))
    (lift:ensure (cl-ppcre:scan "<lfm status=\"ok\">" response))
    (lift:ensure (cl-ppcre:scan "<recenttracks user=" response))
    (let ((sum 0))
      (cl-ppcre:do-matches (s e "<track" response)
        (incf sum))
      (lift:ensure (= sum 5)))))


;; Get shouts


(lift:addtest (cl-lastfm-test)
  test-user-get-shouts-without-api-key
  (handler-case 
      (cl-lastfm:user-get-shouts nil "rj")
    (cl-lastfm:lastfm-request-error (condition)
      (lift:ensure (equal (type-of condition)
                          'cl-lastfm:lastfm-request-error))
      (lift:ensure
       (string-equal "10"
                     (cl-lastfm:request-error-code-of condition))))))


(lift:addtest (cl-lastfm-test)
  test-user-get-shouts
  (let ((response
         (cl-lastfm:user-get-shouts "b25b959554ed76058ac220b7b2e0a026" "rj")))
    (lift:ensure (cl-ppcre:scan "<lfm status=\"ok\">" response))
    (lift:ensure (cl-ppcre:scan "<shouts user=" response))
    (let ((sum 0))
      (cl-ppcre:do-matches (s e "<shout" response)
        (incf sum))
      (lift:ensure (> sum 0)))))



;; Weekly album

(lift:addtest (cl-lastfm-test)
  test-user-get-weekly-album-without-api-key
  (handler-case 
      (cl-lastfm:user-get-weekly-album-chart nil "rj")
    (cl-lastfm:lastfm-request-error (condition)
      (lift:ensure (equal (type-of condition)
                          'cl-lastfm:lastfm-request-error))
      (lift:ensure
       (string-equal "10"
                     (cl-lastfm:request-error-code-of condition))))))


(lift:addtest (cl-lastfm-test)
  test-user-get-weekly-album
  (let ((response
         (cl-lastfm:user-get-weekly-album-chart
          "b25b959554ed76058ac220b7b2e0a026" "rj")))
    (lift:ensure (cl-ppcre:scan "<lfm status=\"ok\">" response))
    (lift:ensure (cl-ppcre:scan "<weeklyalbumchart user=" response))
    (let ((sum 0))
      (cl-ppcre:do-matches (s e "<album" response)
        (incf sum))
      (lift:ensure (> sum 0)))))




;; Weekly artist


(lift:addtest (cl-lastfm-test)
  test-user-get-weekly-artist-without-api-key
  (handler-case 
      (cl-lastfm:user-get-weekly-artist-chart nil "rj")
    (cl-lastfm:lastfm-request-error (condition)
      (lift:ensure (equal (type-of condition)
                          'cl-lastfm:lastfm-request-error))
      (lift:ensure
       (string-equal "10"
                     (cl-lastfm:request-error-code-of condition))))))


(lift:addtest (cl-lastfm-test)
  test-user-get-weekly-artist
  (let ((response
         (cl-lastfm:user-get-weekly-album-chart
          "b25b959554ed76058ac220b7b2e0a026" "rj")))
    (lift:ensure (cl-ppcre:scan "<lfm status=\"ok\">" response))
    (lift:ensure (cl-ppcre:scan "<weeklyartistchart user=" response))
    (lift:ensure (cl-ppcre:scan "</weeklyartistchart>" response))
    (let ((sum 0))
      (cl-ppcre:do-matches (s e "<artist" response)
        (incf sum))
      (lift:ensure (> sum 0)))))



;; Weekly charts


(lift:addtest (cl-lastfm-test)
  test-user-get-weekly-chart-list-without-api-key
  (handler-case 
      (cl-lastfm:user-get-weekly-chart-list nil "rj")
    (cl-lastfm:lastfm-request-error (condition)
      (lift:ensure (equal (type-of condition)
                          'cl-lastfm:lastfm-request-error))
      (lift:ensure
       (string-equal "10"
                     (cl-lastfm:request-error-code-of condition))))))


(lift:addtest (cl-lastfm-test)
  test-user-get-weekly-chart-list
  (let ((response
         (cl-lastfm:user-get-weekly-chart-list
          "b25b959554ed76058ac220b7b2e0a026" "rj")))
    (lift:ensure (cl-ppcre:scan "<lfm status=\"ok\">" response))
    (lift:ensure (cl-ppcre:scan "<weeklyartistchart user=" response))
    (lift:ensure (cl-ppcre:scan "</weeklyartistchart>" response))
    (let ((sum 0))
      (cl-ppcre:do-matches (s e "<chart" response)
        (incf sum))
      (lift:ensure (> sum 0)))))


;; Weekly track chart


(lift:addtest (cl-lastfm-test)
  test-user-get-weekly-track-chart-without-api-key
  (handler-case
      (cl-lastfm:user-get-weekly-track-chart nil "rj")
    (cl-lastfm:lastfm-request-error (condition)
      (lift:ensure (equal (type-of condition)
                          'cl-lastfm:lastfm-request-error))
      (lift:ensure
       (string-equal "10"
                     (cl-lastfm:request-error-code-of condition))))))


(lift:addtest (cl-lastfm-test)
  test-user-get-weekly-track-chart
  (let ((response 
         (cl-lastfm:user-get-weekly-track-chart
          "b25b959554ed76058ac220b7b2e0a026" "rj")))
    (lift:ensure (cl-ppcre:scan "<lfm status=\"ok\">" response))
    (lift:ensure (cl-ppcre:scan "<weeklytrackchart user=" response))
    (lift:ensure (cl-ppcre:scan "</weeklytrackchart>" response))
    (let ((sum 0))
      (cl-ppcre:do-matches (s e "<track" response)
        (incf sum))
      (lift:ensure (> sum 0)))))







