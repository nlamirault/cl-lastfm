;;
;; Geo unit tests
;;

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
