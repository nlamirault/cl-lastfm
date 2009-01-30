;;;;
;;;; Name:          track.lisp
;;;; Purpose:       cl-lastfm unit tests for Track module of LastFM API.
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

;; Get Info


(lift:addtest (cl-lastfm-test)
  test-track-get-info-without-api-key
  (handler-case 
      (cl-lastfm:track-get-info ""
                                :artist "Cher" :track "Believe")
    (cl-lastfm:lastfm-request-error (condition)
      (lift:ensure (equal (type-of condition)
                          'cl-lastfm:lastfm-request-error))
      (lift:ensure
       (string-equal "10"
                     (cl-lastfm:request-error-code-of condition))))))


(lift:addtest (cl-lastfm-test)
  test-track-get-info-with-artist-and-track
  (let ((response
         (cl-lastfm:track-get-info "b25b959554ed76058ac220b7b2e0a026"
                                   :artist "Cher" :track "Believe")))
    (lift:ensure (cl-ppcre:scan "<lfm status=\"ok\">" response))
    (lift:ensure (cl-ppcre:scan "<track>" response))
    (lift:ensure (cl-ppcre:scan "<name>Cher</name>" response))
    (lift:ensure (cl-ppcre:scan "<title>Believe</title>" response))))


;; Get similar


(lift:addtest (cl-lastfm-test)
  test-track-get-similar-without-api-key
  (handler-case 
      (cl-lastfm:track-get-similar ""
                                   :artist "Cher" :track "Believe")
    (cl-lastfm:lastfm-request-error (condition)
      (lift:ensure (equal (type-of condition)
                          'cl-lastfm:lastfm-request-error))
      (lift:ensure
       (string-equal "10"
                     (cl-lastfm:request-error-code-of condition))))))


(lift:addtest (cl-lastfm-test)
  test-track-get-similar-with-artist-and-track
  (let ((response
         (cl-lastfm:track-get-similar "b25b959554ed76058ac220b7b2e0a026" 
                                      :artist "Cher" :track "Believe")))
    (lift:ensure (cl-ppcre:scan "<lfm status=\"ok\">" response))
    (lift:ensure (cl-ppcre:scan "<similartracks track=\"Believe\" artist=\"Cher\""
                                response))
    (let ((sum 0))
      (cl-ppcre:do-matches (s e "<track>" response)
        (incf sum))
      (lift:ensure (> sum 0)))))



;; PB : LastFM don t return the mbid

;; (lift:addtest (cl-lastfm-test)
;;   test-track-get-similar-with-mbid
;;   (let ((response
;;          (cl-lastfm:track-get-similar
;;           "b25b959554ed76058ac220b7b2e0a026"
;;           :mbid "79239441-bfd5-4981-a70c-55c3f15c1287")))
;;     (lift:ensure (cl-ppcre:scan "<lfm status=\"ok\">" response))
;;     (lift:ensure (cl-ppcre:scan "<similartracks track=\"Believe\" artist=\"Cher\""
;;                                 response))
;;     (let ((sum 0))
;;       (cl-ppcre:do-matches (s e "<track>" response)
;;         (incf sum))
;;       (lift:ensure (> sum 0)))))



;; Get top fans


(lift:addtest (cl-lastfm-test)
  test-track-get-top-fans-without-api-key
  (handler-case 
      (cl-lastfm:track-get-top-fans ""
                                    :artist "Cher" :track "Believe")
    (cl-lastfm:lastfm-request-error (condition)
      (lift:ensure (equal (type-of condition)
                          'cl-lastfm:lastfm-request-error))
      (lift:ensure
       (string-equal "10"
                     (cl-lastfm:request-error-code-of condition))))))


(lift:addtest (cl-lastfm-test)
  test-track-get-top-fans-with-artist-and-track
  (let ((response
         (cl-lastfm:track-get-top-fans "b25b959554ed76058ac220b7b2e0a026"
                                       :artist "Cher" :track "Believe")))
    (lift:ensure (cl-ppcre:scan "<lfm status=\"ok\">" response))
    (lift:ensure (cl-ppcre:scan "<topfans " response))
    (let ((sum 0))
      (cl-ppcre:do-matches (s e "<user>" response)
        (incf sum))
      (lift:ensure (> sum 0)))))





;; Get top tags


(lift:addtest (cl-lastfm-test)
  test-track-get-top-tags-without-api-key
  (handler-case 
      (cl-lastfm:track-get-top-tags ""
                                    :artist "Cher" :track "Believe")
    (cl-lastfm:lastfm-request-error (condition)
      (lift:ensure (equal (type-of condition)
                          'cl-lastfm:lastfm-request-error))
      (lift:ensure
       (string-equal "10"
                     (cl-lastfm:request-error-code-of condition))))))


(lift:addtest (cl-lastfm-test)
  test-track-get-top-tags-with-artist-and-track
  (let ((response
         (cl-lastfm:track-get-top-tags "b25b959554ed76058ac220b7b2e0a026"
                                       :artist "Cher" :track "Believe")))
    (lift:ensure (cl-ppcre:scan "<lfm status=\"ok\">" response))
    (lift:ensure (cl-ppcre:scan "<toptags " response))
    (let ((sum 0))
      (cl-ppcre:do-matches (s e "<tag>" response)
        (incf sum))
      (lift:ensure (> sum 0)))))



;; search


(lift:addtest (cl-lastfm-test)
  test-track-search-without-api-key
  (handler-case 
      (cl-lastfm:track-search ""
                              :artist "Cher" :track "Believe")
    (cl-lastfm:lastfm-request-error (condition)
      (lift:ensure (equal (type-of condition)
                          'cl-lastfm:lastfm-request-error))
      (lift:ensure
       (string-equal "10"
                     (cl-lastfm:request-error-code-of condition))))))


(lift:addtest (cl-lastfm-test)
  test-track-search
  (let ((response
         (cl-lastfm:track-search "b25b959554ed76058ac220b7b2e0a026"
                                 "Tostaki")))
    (lift:ensure (cl-ppcre:scan "<lfm status=\"ok\">" response))
    (lift:ensure (cl-ppcre:scan "<results for " response))
    (lift:ensure (cl-ppcre:scan "<trackmatches>" response))
    (let ((sum 0))
      (cl-ppcre:do-matches (s e "<track>" response)
        (incf sum))
      (lift:ensure (> sum 0)))))



