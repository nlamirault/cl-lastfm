;;
;; Users tests
;;

(in-package #:cl-lastfm-test)

(define-test cant-get-user-events-without-api-key
  (handler-case
      (cl-lastfm:user-get-events nil "rj")
    (cl-lastfm:lastfm-request-error (condition)
      (assert-error 'lastfm-request-error condition)
      (assert-equal "10" (request-error-code-of condition)))))


;; FIXME: find API Key
;; (define-test can-get-user-events-with-name
;;   (let ((response
;;          (user-get-events "b25b959554ed76058ac220b7b2e0a026"
;; 			  "rj")))
;;     (assert-true (cl-ppcre:scan "<lfm status=\"ok\">" response))
;;     (assert-true (cl-ppcre:scan "<events user=" response))
;;     (let ((sum 0))
;;       (cl-ppcre:do-matches (s e "<user>" response)
;;         (incf sum))
;;       (assert-true (>= sum 0)))))
