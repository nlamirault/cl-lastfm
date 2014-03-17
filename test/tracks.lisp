;; Copyright (C) 2014  Nicolas Lamirault

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


(in-package #:cl-lastfm-test)

(define-test get-track-info
  (let ((response
         (cl-lastfm:track-get-info *api-key*
				   :artist "Cher" :track "Believe")))
    (assert-true (cl-ppcre:scan "<lfm status=\"ok\">" response))
    (assert-true (cl-ppcre:scan "<track>" response))
    (assert-true (cl-ppcre:scan "<name>Cher</name>" response))
    (assert-true (cl-ppcre:scan "<title>The Very Best of Cher</title>" response))
    (assert-true (cl-ppcre:scan "<name>Believe</name>" response))))

(define-test get-track-similar
  (let ((sum 0)
	(response
         (cl-lastfm:track-get-similar *api-key*
				      :artist "Cher"
				      :track "Believe")))
    (assert-true (cl-ppcre:scan "<lfm status=\"ok\">" response))
    (assert-true (cl-ppcre:scan "<similartracks track=\"Believe\" artist=\"Cher\">"
                                response))
    (cl-ppcre:do-matches (s e "<track>" response)
      (incf sum))
    (assert-true (> sum 0))))

(define-test get-track-top-fans
  (let ((sum 0)
	(response
         (cl-lastfm:track-get-top-fans *api-key*
				       :artist "Cher"
				       :track "Believe")))
    (assert-true (cl-ppcre:scan "<lfm status=\"ok\">" response))
    (assert-true (cl-ppcre:scan "<topfans artist=\"Cher\" track=\"Believe\">"
                                response))
    (cl-ppcre:do-matches (s e "<user>" response)
      (incf sum))
    (assert-true (> sum 0))))


(define-test get-track-top-tags
  (let ((sum 0)
	(response
         (cl-lastfm:track-get-top-tags *api-key*
				       :artist "Cher"
				       :track "Believe")))
    (assert-true (cl-ppcre:scan "<lfm status=\"ok\">" response))
    (assert-true (cl-ppcre:scan "<toptags artist=\"Cher\" track=\"Believe\">"
                                response))
    (cl-ppcre:do-matches (s e "<tag>" response)
      (incf sum))
    (assert-true (> sum 0))))

(define-test search-track
  (let ((response (cl-lastfm:track-search *api-key* "Tostaki")))
    (assert-true (cl-ppcre:scan "<lfm status=\"ok\">" response))
    (assert-true (cl-ppcre:scan "<results for=\"Tostaki\"" response))
    (assert-true (cl-ppcre:scan "<artist>Noir Désir</artist>" response))))
