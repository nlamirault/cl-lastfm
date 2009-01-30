;;;;
;;;; Name:          library.lisp
;;;; Purpose:       cl-lastfm unit tests for Library module of LastFM API.
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



;; Get Albums


(lift:addtest (cl-lastfm-test)
  test-library-get-albums-without-api-key
  (handler-case 
      (cl-lastfm:library-get-albums "" "lam_")                                
    (cl-lastfm:lastfm-request-error (condition)
      (lift:ensure (equal (type-of condition)
                          'cl-lastfm:lastfm-request-error))
      (lift:ensure
       (string-equal "10"
                     (cl-lastfm:request-error-code-of condition))))))



(lift:addtest (cl-lastfm-test)
  test-library-get-albums-with-custom-limit
  (let ((response
         (cl-lastfm:library-get-albums "b25b959554ed76058ac220b7b2e0a026"
                                       "lam_" :limit 5)))
    (lift:ensure (cl-ppcre:scan "<lfm status=\"ok\">" response))
    (lift:ensure (cl-ppcre:scan "<albums user= " response))
    (let ((sum 0))
      (cl-ppcre:do-matches (s e "<album>" response)
        (incf sum))
      (lift:ensure (= 5 sum)))))



;; Get artists


(lift:addtest (cl-lastfm-test)
  test-library-get-artists-without-api-key
  (handler-case 
      (cl-lastfm:library-get-artists "" "lam_")                                
    (cl-lastfm:lastfm-request-error (condition)
      (lift:ensure (equal (type-of condition)
                          'cl-lastfm:lastfm-request-error))
      (lift:ensure
       (string-equal "10"
                     (cl-lastfm:request-error-code-of condition))))))



(lift:addtest (cl-lastfm-test)
  test-library-get-artists-with-custom-limit
  (let ((response
         (cl-lastfm:library-get-artists "b25b959554ed76058ac220b7b2e0a026"
                                        "lam_" :limit 5)))
    (lift:ensure (cl-ppcre:scan "<lfm status=\"ok\">" response))
    (lift:ensure (cl-ppcre:scan "<artists user= " response))
    (let ((sum 0))
      (cl-ppcre:do-matches (s e "<artist>" response)
        (incf sum))
      (lift:ensure (= 5 sum)))))



;; Get tracks


(lift:addtest (cl-lastfm-test)
  test-library-get-tracks-without-api-key
  (handler-case 
      (cl-lastfm:library-get-tracks "" "lam_")                                
    (cl-lastfm:lastfm-request-error (condition)
      (lift:ensure (equal (type-of condition)
                          'cl-lastfm:lastfm-request-error))
      (lift:ensure
       (string-equal "10"
                     (cl-lastfm:request-error-code-of condition))))))



(lift:addtest (cl-lastfm-test)
  test-library-get-tracks-with-custom-limit
  (let ((response
         (cl-lastfm:library-get-tracks "b25b959554ed76058ac220b7b2e0a026"
                                        "lam_" :limit 5)))
    (lift:ensure (cl-ppcre:scan "<lfm status=\"ok\">" response))
    (lift:ensure (cl-ppcre:scan "<tracks user= " response))
    (let ((sum 0))
      (cl-ppcre:do-matches (s e "<track>" response)
        (incf sum))
      (lift:ensure (= 5 sum)))))

