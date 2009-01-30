;;;;
;;;; Name:          tag.lisp
;;;; Purpose:       cl-lastfm unit tests for Tag module of LastFM API.
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



;; Get similar tags


(lift:addtest (cl-lastfm-test)
  test-tag-get-similar-without-api-key
  (handler-case 
      (cl-lastfm:tag-get-similar "" "rock")
    (cl-lastfm:lastfm-request-error (condition)
      (lift:ensure (equal (type-of condition)
                          'cl-lastfm:lastfm-request-error))
      (lift:ensure
       (string-equal "10"
                     (cl-lastfm:request-error-code-of condition))))))



(lift:addtest (cl-lastfm-test)
  test-tag-get-similar
  (let ((response
         (cl-lastfm:tag-get-similar "b25b959554ed76058ac220b7b2e0a026"
                                    "rock")))
    (lift:ensure (cl-ppcre:scan "<lfm status=\"ok\">" response))
    (lift:ensure (cl-ppcre:scan "<similartags tag=\"rock\" " response))
    (let ((sum 0))
      (cl-ppcre:do-matches (s e "<tag>" response)
        (incf sum))
      (lift:ensure (> sum 0)))))



;; Get top albums


(lift:addtest (cl-lastfm-test)
  test-tag-get-top-albums-without-api-key
  (handler-case 
      (cl-lastfm:tag-get-top-albums "" "rock")
    (cl-lastfm:lastfm-request-error (condition)
      (lift:ensure (equal (type-of condition)
                          'cl-lastfm:lastfm-request-error))
      (lift:ensure
       (string-equal "10"
                     (cl-lastfm:request-error-code-of condition))))))



(lift:addtest (cl-lastfm-test)
  test-tag-get-top-albums
  (let ((response
         (cl-lastfm:tag-get-top-albums "b25b959554ed76058ac220b7b2e0a026"
                                       "rock")
    (lift:ensure (cl-ppcre:scan "<lfm status=\"ok\">" response))
    (lift:ensure (cl-ppcre:scan "<topalbums tag=\"rock\" " response))
    (let ((sum 0))
      (cl-ppcre:do-matches (s e "<album rank=" response)
        (incf sum))
      (lift:ensure (> sum 0)))))




;; Get top artists


(lift:addtest (cl-lastfm-test)
  test-tag-get-top-artists-without-api-key
  (handler-case 
      (cl-lastfm:tag-get-top-artists "" "rock")
    (cl-lastfm:lastfm-request-error (condition)
      (lift:ensure (equal (type-of condition)
                          'cl-lastfm:lastfm-request-error))
      (lift:ensure
       (string-equal "10"
                     (cl-lastfm:request-error-code-of condition))))))



(lift:addtest (cl-lastfm-test)
  test-tag-get-top-artists
  (let ((response
         (cl-lastfm:tag-get-top-artists "b25b959554ed76058ac220b7b2e0a026"
                                        "rock")
    (lift:ensure (cl-ppcre:scan "<lfm status=\"ok\">" response))
    (lift:ensure (cl-ppcre:scan "<topartists tag=\"rock\" " response))
    (let ((sum 0))
      (cl-ppcre:do-matches (s e "<artist rank=" response)
        (incf sum))
      (lift:ensure (> sum 0)))))




;; Get top tracks


(lift:addtest (cl-lastfm-test)
  test-tag-get-top-tracks-without-api-key
  (handler-case 
      (cl-lastfm:tag-get-top-tracks "" "rock")
    (cl-lastfm:lastfm-request-error (condition)
      (lift:ensure (equal (type-of condition)
                          'cl-lastfm:lastfm-request-error))
      (lift:ensure
       (string-equal "10"
                     (cl-lastfm:request-error-code-of condition))))))



(lift:addtest (cl-lastfm-test)
  test-tag-get-top-tracks
  (let ((response
         (cl-lastfm:tag-get-top-tracks "b25b959554ed76058ac220b7b2e0a026"
                                       "rock")
    (lift:ensure (cl-ppcre:scan "<lfm status=\"ok\">" response))
    (lift:ensure (cl-ppcre:scan "<toptracks tag=\"rock\" " response))
    (let ((sum 0))
      (cl-ppcre:do-matches (s e "<track rank=" response)
        (incf sum))
      (lift:ensure (> sum 0)))))





;; Get top tags


(lift:addtest (cl-lastfm-test)
  test-tag-get-top-tags-without-api-key
  (handler-case 
      (cl-lastfm:tag-get-top-tags "" "rock")
    (cl-lastfm:lastfm-request-error (condition)
      (lift:ensure (equal (type-of condition)
                          'cl-lastfm:lastfm-request-error))
      (lift:ensure
       (string-equal "10"
                     (cl-lastfm:request-error-code-of condition))))))



(lift:addtest (cl-lastfm-test)
  test-tag-get-top-tags
  (let ((response
         (cl-lastfm:tag-get-top-tags "b25b959554ed76058ac220b7b2e0a026")))
    (lift:ensure (cl-ppcre:scan "<lfm status=\"ok\">" response))
    (lift:ensure (cl-ppcre:scan "<toptags>" response))
    (let ((sum 0))
      (cl-ppcre:do-matches (s e "<tag>" response)
        (incf sum))
      (lift:ensure (> sum 0)))))




;; Get Weekly artist chart



(lift:addtest (cl-lastfm-test)
  test-tag-get-weekly-artist-chart-without-api-key
  (handler-case 
      (cl-lastfm:tag-get-weekly-artist-chart "" "rock")
    (cl-lastfm:lastfm-request-error (condition)
      (lift:ensure (equal (type-of condition)
                          'cl-lastfm:lastfm-request-error))
      (lift:ensure
       (string-equal "10"
                     (cl-lastfm:request-error-code-of condition))))))



(lift:addtest (cl-lastfm-test)
  test-tag-get-weekly-artist-chart
  (let ((response
         (cl-lastfm:tag-get-weekly-artist-chart
          "b25b959554ed76058ac220b7b2e0a026" "rock" :limit 5)))
    (lift:ensure (cl-ppcre:scan "<lfm status=\"ok\">" response))
    (lift:ensure (cl-ppcre:scan "<weeklyartistchart tag=\"rock\" " response))
    (let ((sum 0))
      (cl-ppcre:do-matches (s e "<artist rank=" response)
        (incf sum))
      (lift:ensure (= sum 5)))))




;; Get Weekly chart list



(lift:addtest (cl-lastfm-test)
  test-tag-get-weekly-chart-list-without-api-key
  (handler-case 
      (cl-lastfm:tag-get-weekly-chart-list "" "rock")
    (cl-lastfm:lastfm-request-error (condition)
      (lift:ensure (equal (type-of condition)
                          'cl-lastfm:lastfm-request-error))
      (lift:ensure
       (string-equal "10"
                     (cl-lastfm:request-error-code-of condition))))))


(lift:addtest (cl-lastfm-test)
  test-tag-get-weekly-chart-list
  (let ((response
         (cl-lastfm:tag-get-weekly-chart-list
          "b25b959554ed76058ac220b7b2e0a026" "rock")))
    (lift:ensure (cl-ppcre:scan "<lfm status=\"ok\">" response))
    (lift:ensure (cl-ppcre:scan "<weeklychartlist tag=\"rock\" " response))
    (let ((sum 0))
      (cl-ppcre:do-matches (s e "<chart " response)
        (incf sum))
      (lift:ensure (> sum 0)))))





;; Search



(lift:addtest (cl-lastfm-test)
  test-tag-search-without-api-key
  (handler-case 
      (cl-lastfm:tag-search "" "rock")
    (cl-lastfm:lastfm-request-error (condition)
      (lift:ensure (equal (type-of condition)
                          'cl-lastfm:lastfm-request-error))
      (lift:ensure
       (string-equal "10"
                     (cl-lastfm:request-error-code-of condition))))))


(lift:addtest (cl-lastfm-test)
  test-tag-search
  (let ((response
         (cl-lastfm:tag-search
          "b25b959554ed76058ac220b7b2e0a026" "rock")))
    (lift:ensure (cl-ppcre:scan "<lfm status=\"ok\">" response))
    (lift:ensure (cl-ppcre:scan "<results for" response))
    (let ((sum 0))
      (cl-ppcre:do-matches (s e "<tag>" response)
        (incf sum))
      (lift:ensure (> sum 0)))))




