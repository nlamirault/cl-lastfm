;;
;; Tasteomter unit tests
;;


(in-package #:cl-lastfm-test)

(define-test tasteometer-compare-user
  (let ((sum 0)
	(response
         (cl-lastfm:tasteometer-compare *api-key*
					"user" "user" "rj" "lam_")))
    (assert-true (cl-ppcre:scan "<lfm status=\"ok\">" response))
    (assert-true (cl-ppcre:scan "<comparison>" response))
    (assert-true (cl-ppcre:scan "<artists matches=" response))
    (assert-true (cl-ppcre:scan "<name>RJ</name>" response))
    (assert-true (cl-ppcre:scan "<name>lam_</name>" response))))
