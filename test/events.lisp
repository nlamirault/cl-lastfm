;;;;
;;;; Name:          events.lisp
;;;; Purpose:       cl-lastfm unit tests for Events module of LastFM API.
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



;; Get Infos


(lift:addtest (cl-lastfm-test)
  test-event-get-info-without-api-key
  (handler-case 
      (cl-lastfm:event-get-info nil "" "328799")                                
    (cl-lastfm:lastfm-request-error (condition)
      (lift:ensure (equal (type-of condition)
                          'cl-lastfm:lastfm-request-error))
      (lift:ensure
       (string-equal "10"
                     (cl-lastfm:request-error-code-of condition))))))



(lift:addtest (cl-lastfm-test)
  test-event-get-info
  (let ((response
         (cl-lastfm:event-get-info "b25b959554ed76058ac220b7b2e0a026"
                                   "328799")))
    (lift:ensure (cl-ppcre:scan "<lfm status=\"ok\">" response))
    (lift:ensure (cl-ppcre:scan "<event " response))
    (lift:ensure (cl-ppcre:scan "<id>328799</id>" response))
    (lift:ensure (cl-ppcre:scan "<artist>Philip Glass</artist>" response))
    (lift:ensure (cl-ppcre:scan
                  "<artist>Orchestra and Chorus of Erfurt Theatre</artist>"
                  response))))



;; Get Shouts

(lift:addtest (cl-lastfm-test)
  test-event-get-shouts-without-api-key
  (handler-case 
      (cl-lastfm:event-get-shouts nil "" "328799")                                
    (cl-lastfm:lastfm-request-error (condition)
      (lift:ensure (equal (type-of condition)
                          'cl-lastfm:lastfm-request-error))
      (lift:ensure
       (string-equal "10"
                     (cl-lastfm:request-error-code-of condition))))))



(lift:addtest (cl-lastfm-test)
  test-event-get-info
  (let ((response
         (cl-lastfm:event-get-shouts "b25b959554ed76058ac220b7b2e0a026"
                                   "328799")))
    (lift:ensure (cl-ppcre:scan "<lfm status=\"ok\">" response))
    (lift:ensure (cl-ppcre:scan "<shouts event" response))))




