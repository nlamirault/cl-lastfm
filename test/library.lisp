;;
;; Library unit tests
;;

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
