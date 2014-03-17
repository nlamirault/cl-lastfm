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


(define-test test-album-get-info-without-api-key
  (handler-case
      (album-get-info "" :artist-name "U2" :album-name "War")
    (lastfm-request-error (condition)
      (assert-error 'lastfm-request-error condition)
      (assert-equal "10" (request-error-code-of condition)))))

(define-test test-album-get-info-without-artist
  (handler-case
      (cl-lastfm:album-get-info *api-key*
				:album-name "War")
    (lastfm-request-error (condition)
      (assert-error 'lastfm-request-error condition)
      (assert-equal "6" (request-error-code-of condition)))))

(define-test test-album-get-info
  (let ((response
         (cl-lastfm:album-get-info *api-key*
				   :artist-name "U2"
				   :album-name "War")))
    (assert-true (cl-ppcre:scan "<lfm status=\"ok\">" response))
    (assert-true (cl-ppcre:scan "<album>" response))
    (assert-true (cl-ppcre:scan "<name>War</name>" response))
    (assert-true (cl-ppcre:scan "<artist>U2</artist>" response))
    (assert-true (cl-ppcre:scan "<mbid>1be68d56-dbbd-3e4e-9fa8-98a71dc63ff3</mbid>"
				response))))

(define-test test-album-get-info-by-mbid
  (let ((response
         (cl-lastfm:album-get-info *api-key*
				   :mbid "1be68d56-dbbd-3e4e-9fa8-98a71dc63ff3")))
    (assert-true (cl-ppcre:scan "<lfm status=\"ok\">" response))
    (assert-true (cl-ppcre:scan "<album>" response))
    (assert-true (cl-ppcre:scan "<name>War</name>" response))
    (assert-true (cl-ppcre:scan "<artist>U2</artist>" response))
    (assert-true (cl-ppcre:scan "<mbid>1be68d56-dbbd-3e4e-9fa8-98a71dc63ff3</mbid>"
				response))))

(define-test test-search-album
  (let ((response
         (cl-lastfm:album-search *api-key* "Hello")))
    (assert-true (cl-ppcre:scan "<lfm status=\"ok\">" response))
    (assert-true (cl-ppcre:scan "<results for=\"Hello\" " response))
    (assert-true (cl-ppcre:scan "<name>Hello Nasty</name>" response))
    (assert-true (cl-ppcre:scan "<artist>Beastie Boys</artist>" response))))
