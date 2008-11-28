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
;;
;; Artists
;;



;; Get Events


(lift:addtest (cl-lastfm-test)
  test-artist-get-events-without-api-key
  (handler-case 
      (cl-lastfm:artist-get-events nil "cher")
    (cl-lastfm:lastfm-request-error (condition)
      (lift:ensure (equal (type-of condition)
                          'cl-lastfm:lastfm-request-error))
      (lift:ensure
       (string-equal "10"
                     (cl-lastfm:request-error-code-of condition))))))


(lift:addtest (cl-lastfm-test)
  test-artist-get-events
  (let ((response
         (cl-lastfm:artist-get-events "b25b959554ed76058ac220b7b2e0a026"
                                      "Budos Band")))
    (lift:ensure (cl-ppcre:scan "<lfm status=\"ok\">" response))
    (lift:ensure (cl-ppcre:scan "<events artist=\"Budos Band\"" response))))
    
    


;; Get Infos


(lift:addtest (cl-lastfm-test)
  test-artist-get-infos-without-api-key
  (handler-case 
      (cl-lastfm::artist-get-info nil)
    (cl-lastfm:lastfm-request-error (condition)
      (lift:ensure (equal (type-of condition)
                          'cl-lastfm:lastfm-request-error))
      (lift:ensure
       (string-equal "10"
                     (cl-lastfm:request-error-code-of condition))))))


(lift:addtest (cl-lastfm-test)
  test-artist-get-infos-without-parameters
  (handler-case 
      (cl-lastfm::artist-get-info "b25b959554ed76058ac220b7b2e0a026")
    (cl-lastfm:lastfm-request-error (condition)
      (lift:ensure (equal (type-of condition)
                          'cl-lastfm:lastfm-request-error))
      (lift:ensure
       (string-equal "6"
                     (cl-lastfm:request-error-code-of condition))))))


(lift:addtest (cl-lastfm-test)
  test-artist-get-infos-with-name
  (let ((response
         (cl-lastfm::artist-get-info "b25b959554ed76058ac220b7b2e0a026"
                                     :artist-name "The Strokes"
                                     :lang "fr")))
    (lift:ensure (cl-ppcre:scan "<lfm status=\"ok\">" response))
    (lift:ensure (cl-ppcre:scan "<name>The Strokes</name>" response))
    (lift:ensure (cl-ppcre:scan "<mbid>f181961b-20f7-459e-89de-920ef03c7ed0</mbid>"
                                response))))


(lift:addtest (cl-lastfm-test)
  test-artist-get-infos-with-mbid
  (let ((response
         (cl-lastfm::artist-get-info "b25b959554ed76058ac220b7b2e0a026"
                                     :mbid "f181961b-20f7-459e-89de-920ef03c7ed0"
                                     :lang "fr")))
    (lift:ensure (cl-ppcre:scan "<lfm status=\"ok\">" response))
    (lift:ensure (cl-ppcre:scan "<name>The Strokes</name>" response))
    (lift:ensure (cl-ppcre:scan "<mbid>f181961b-20f7-459e-89de-920ef03c7ed0</mbid>"
                                response))))


;; Get Similar artists


(lift:addtest (cl-lastfm-test)
  test-artist-get-similar-without-api-key
  (handler-case 
      (cl-lastfm:artist-get-similar nil "The Strokes")
    (cl-lastfm:lastfm-request-error (condition)
      (lift:ensure (equal (type-of condition)
                          'cl-lastfm:lastfm-request-error))
      (lift:ensure
       (string-equal "10"
                     (cl-lastfm:request-error-code-of condition))))))


(lift:addtest (cl-lastfm-test)
  test-artist-get-similar-with-name
  (let ((response
         (cl-lastfm:artist-get-similar "b25b959554ed76058ac220b7b2e0a026"
                                       "The Strokes")))
    (lift:ensure (cl-ppcre:scan "<lfm status=\"ok\">" response))
    (lift:ensure (cl-ppcre:scan "<similarartists artist=\"The Strokes\">"
                                response))))



(lift:addtest (cl-lastfm-test)
  test-artist-get-similar-with-limit
  (let ((response
         (cl-lastfm:artist-get-similar "b25b959554ed76058ac220b7b2e0a026"
                                       "The Strokes" 2)))
    (lift:ensure (cl-ppcre:scan "<lfm status=\"ok\">" response))
    (lift:ensure (cl-ppcre:scan "<similarartists artist=\"The Strokes\">"
                                response))
    (let ((sum 0))
      (cl-ppcre:do-matches (s e "<artist>" response)
        (incf sum))
      (lift:ensure (= 2 sum)))))



;; Get Top albums


(lift:addtest (cl-lastfm-test)
  test-artist-get-top-albums-without-api-key
  (handler-case 
      (cl-lastfm:artist-get-top-albums nil "The Strokes")
    (cl-lastfm:lastfm-request-error (condition)
      (lift:ensure (equal (type-of condition)
                          'cl-lastfm:lastfm-request-error))
      (lift:ensure
       (string-equal "10"
                     (cl-lastfm:request-error-code-of condition))))))

(lift:addtest (cl-lastfm-test)
  test-artist-get-top-albums-with-name
  (let ((response
         (cl-lastfm:artist-get-top-albums "b25b959554ed76058ac220b7b2e0a026"
                                          "The Strokes")))
    (lift:ensure (cl-ppcre:scan "<lfm status=\"ok\">" response))
    (lift:ensure (cl-ppcre:scan "<topalbums artist=\"The Strokes\" >"
                                response))
    (let ((sum 0))
      (cl-ppcre:do-matches (s e "<album  rank" response)
        (incf sum))
    (lift:ensure (> sum 0)))))



;; Get Top fans


(lift:addtest (cl-lastfm-test)
  test-artist-get-top-fans-without-api-key
  (handler-case 
      (cl-lastfm:artist-get-top-fans nil "The Strokes")
    (cl-lastfm:lastfm-request-error (condition)
      (lift:ensure (equal (type-of condition)
                          'cl-lastfm:lastfm-request-error))
      (lift:ensure
       (string-equal "10"
                     (cl-lastfm:request-error-code-of condition))))))

(lift:addtest (cl-lastfm-test)
  test-artist-get-top-fans-with-name
  (let ((response
         (cl-lastfm:artist-get-top-fans "b25b959554ed76058ac220b7b2e0a026"
                                        "The Strokes")))
    (lift:ensure (cl-ppcre:scan "<lfm status=\"ok\">" response))
    (lift:ensure (cl-ppcre:scan "<topfans artist=\"The Strokes\">"
                                response))
    (let ((sum 0))
      (cl-ppcre:do-matches (s e "<user>" response)
        (incf sum))
    (lift:ensure (> sum 0)))))



;; Get Top tags


(lift:addtest (cl-lastfm-test)
  test-artist-get-top-tags-without-api-key
  (handler-case 
      (cl-lastfm:artist-get-top-tags nil "The Strokes")
    (cl-lastfm:lastfm-request-error (condition)
      (lift:ensure (equal (type-of condition)
                          'cl-lastfm:lastfm-request-error))
      (lift:ensure
       (string-equal "10"
                     (cl-lastfm:request-error-code-of condition))))))

(lift:addtest (cl-lastfm-test)
  test-artist-get-top-tags-with-name
  (let ((response
         (cl-lastfm:artist-get-top-tags "b25b959554ed76058ac220b7b2e0a026"
                                        "The Strokes")))
    (lift:ensure (cl-ppcre:scan "<lfm status=\"ok\">" response))
    (lift:ensure (cl-ppcre:scan "<toptags artist=\"The Strokes\">"
                                response))
    (let ((sum 0))
      (cl-ppcre:do-matches (s e "<tag>" response)
        (incf sum))
    (lift:ensure (> sum 0)))))



;; Get Top tracks


(lift:addtest (cl-lastfm-test)
  test-artist-get-top-tracks-without-api-key
  (handler-case 
      (cl-lastfm:artist-get-top-tracks nil "The Strokes")
    (cl-lastfm:lastfm-request-error (condition)
      (lift:ensure (equal (type-of condition)
                          'cl-lastfm:lastfm-request-error))
      (lift:ensure
       (string-equal "10"
                     (cl-lastfm:request-error-code-of condition))))))

(lift:addtest (cl-lastfm-test)
  test-artist-get-top-tracks-with-name
  (let ((response
         (cl-lastfm:artist-get-top-tracks "b25b959554ed76058ac220b7b2e0a026"
                                          "The Strokes")))
    (lift:ensure (cl-ppcre:scan "<lfm status=\"ok\">" response))
    (lift:ensure (cl-ppcre:scan "<toptracks artist=\"The Strokes\">"
                                response))
    (let ((sum 0))
      (cl-ppcre:do-matches (s e "<track rank=" response)
        (incf sum))
    (lift:ensure (> sum 0)))))



;; Artists search


(lift:addtest (cl-lastfm-test)
  test-artist-search
  (let ((response
         (cl-lastfm::artist-search "b25b959554ed76058ac220b7b2e0a026"
                                   "Strokes" 
                                   :limit 10)))
    (lift:ensure (cl-ppcre:scan "<lfm status=\"ok\">" response))
    (let ((sum 0))
      (cl-ppcre:do-matches (s e "<artist>" response)
        (incf sum))
      (lift:ensure (= 10 sum)))))




  
