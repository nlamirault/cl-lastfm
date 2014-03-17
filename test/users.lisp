;; Copyright (C) 2014  Nicolas Lamirault

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
;; THE SOFTWARE.

(in-package #:cl-lastfm-test)


(define-test cant-get-user-events-without-api-key
  (handler-case
      (user-get-events "" "rj")
    (cl-lastfm:lastfm-request-error (condition)
      (assert-error 'lastfm-request-error condition)
      (assert-equal "10" (request-error-code-of condition)))))


(define-test can-get-user-events-with-name
  (format t "API Key: ~A" (sb-posix:getenv "CL-LASTFM-API-KEY"))
  (let ((sum 0)
	(response (user-get-events *api-key* "rj")))
    (assert-true (cl-ppcre:scan "<lfm status=\"ok\">" response))
    (assert-true (cl-ppcre:scan "<events " response))
    (cl-ppcre:do-matches (s e "<user>" response)
      (incf sum))
    (assert-true (>= sum 0))))

(define-test test-event-get-info
  (let ((response
         (cl-lastfm:event-get-info *api-key* "328799")))
    (assert-true (cl-ppcre:scan "<lfm status=\"ok\">" response))
    (assert-true (cl-ppcre:scan "<event " response))
    (assert-true (cl-ppcre:scan "<id>328799</id>" response))
    (assert-true (cl-ppcre:scan "<artist>Philip Glass</artist>" response))
    (assert-true (cl-ppcre:scan
                  "<artist>Orchestra and Chorus of Erfurt Theatre</artist>"
                  response))))

(define-test test-user-friends
  (let ((sum 0)
	(response
         (cl-lastfm:user-get-friends *api-key* "rj")))
    (assert-true (cl-ppcre:scan "<lfm status=\"ok\">" response))
    (assert-true (cl-ppcre:scan "<friends for=\"RJ\" " response))
    (cl-ppcre:do-matches (s e "<user>" response)
      (incf sum))
    (assert-true (>= sum 0))))
