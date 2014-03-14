;;
;; Tracks unit tests
;;


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
