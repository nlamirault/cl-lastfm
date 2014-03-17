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


(define-test test-library-get-albums-with-custom-limit
  (let* ((limit 5)
	 (response
	  (cl-lastfm:library-get-albums *api-key* "lam_" :limit limit))
	(sum 0))
    (assert-true (cl-ppcre:scan "<lfm status=\"ok\">" response))
    (assert-true (cl-ppcre:scan "<albums user=" response))
    (cl-ppcre:do-matches (s e "<album>" response)
      (incf sum))
    (assert-equal limit sum)))

(define-test test-library-get-artists-with-custom-limit
  (let* ((limit 10)
	 (response
	  (cl-lastfm:library-get-artists *api-key* "lam_" :limit limit))
	(sum 0))
    (assert-true (cl-ppcre:scan "<lfm status=\"ok\">" response))
    (assert-true (cl-ppcre:scan "<artists user=" response))
    (cl-ppcre:do-matches (s e "<artist>" response)
      (incf sum))
    (assert-equal limit sum)))

(define-test test-library-get-tracks-with-custom-limit
    (let* ((limit 10)
	   (response
	    (cl-lastfm:library-get-tracks *api-key* "lam_" :limit limit))
	   (sum 0))
      (assert-true (cl-ppcre:scan "<lfm status=\"ok\">" response))
      (assert-true (cl-ppcre:scan "<tracks user=" response))
      (cl-ppcre:do-matches (s e "<track>" response)
	(incf sum))
      (assert-equal limit sum)))
