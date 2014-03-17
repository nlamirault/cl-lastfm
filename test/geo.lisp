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


(define-test geo-get-events-with-location-name
  (let ((sum 0)
	(response
         (cl-lastfm:geo-get-events *api-key* :location "Bordeaux")))
    (assert-true (cl-ppcre:scan "<lfm status=\"ok\">" response))
    (assert-true (cl-ppcre:scan "<events " response))
    (assert-true (cl-ppcre:scan "location=\"Bordeaux, France\"" response))
    (cl-ppcre:do-matches (s e "<event " response)
      (incf sum))
    (assert-true (> sum 0))))


(define-test geo-get-events-with-latitude-longitude
  (let ((sum 0)
	(response
         (cl-lastfm:geo-get-events *api-key*
				   :lat "44.8333333"
                                   :long "-0.5666667")))
    (assert-true (cl-ppcre:scan "<lfm status=\"ok\">" response))
    (assert-true (cl-ppcre:scan "<events " response))
    (cl-ppcre:do-matches (s e "<event " response)
      (incf sum))
    (assert-true (> sum 0))))

(define-test geo-top-artists
  (let ((sum 0)
	(response
         (cl-lastfm:geo-top-artists *api-key* "France")))
    (assert-true (cl-ppcre:scan "<lfm status=\"ok\">" response))
    (assert-true (cl-ppcre:scan "<topartists country=\"France\""
                                response))
    (cl-ppcre:do-matches (s e "<artist rank" response)
      (incf sum))
    (assert-true (> sum 0))))

(define-test geo-top-tracks
  (let ((sum 0)
	(response
         (cl-lastfm:geo-top-tracks *api-key* "France")))
    (assert-true (cl-ppcre:scan "<lfm status=\"ok\">" response))
    (assert-true (cl-ppcre:scan "<toptracks country=\"France\""
                                response))
    (cl-ppcre:do-matches (s e "<track rank" response)
      (incf sum))
    (assert-true (> sum 0))))
