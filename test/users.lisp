;;
;; Users tests
;;

(in-package #:cl-lastfm-test)


(defparameter *api-key* (sb-posix:getenv "CL-LASTFM-API-KEY"))


(define-test cant-get-user-events-without-api-key
  (handler-case
      (cl-lastfm:user-get-events nil "rj")
    (cl-lastfm:lastfm-request-error (condition)
      (assert-error 'lastfm-request-error condition)
      (assert-equal "10" (request-error-code-of condition)))))


(define-test can-get-user-events-with-name
  (let ((response
         (user-get-events *api-key*
			  "rj")))
    (assert-true (cl-ppcre:scan "<lfm status=\"ok\">" response))
    (assert-true (cl-ppcre:scan "<events " response))
    (let ((sum 0))
      (cl-ppcre:do-matches (s e "<user>" response)
        (incf sum))
      (assert-true (>= sum 0)))))
