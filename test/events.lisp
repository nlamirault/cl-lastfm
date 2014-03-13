;;
;; events tests
;;

(in-package #:cl-lastfm-test)


(define-test test-event-get-info-without-api-key
  (handler-case
      (event-get-info "" "328799")
    (lastfm-request-error (condition)
      (assert-error 'lastfm-request-error condition)
      (assert-equal "10" (request-error-code-of condition)))))

(define-test test-event-get-info
  (let ((response
         (cl-lastfm:event-get-info *api-key* "328799")))
    (assert-true (cl-ppcre:scan "<lfm status=\"ok\">" response))
    (assert-true (cl-ppcre:scan "<event " response))
    (assert-true (cl-ppcre:scan "<id>328799</id>" response))
    (assert-true (cl-ppcre:scan "<artist>Philip Glass</artist>" response))
    (assert-true (cl-ppcre:scan
                  "<artist>Orchestra and Chorus of Erfurt Theatre</artist>"
                  response))))

(define-test test-event-get-shouts-without-api-key
  (handler-case
      (event-get-shouts "" "328799")
    (lastfm-request-error (condition)
      (assert-error 'lastfm-request-error condition)
      (assert-equal "10" (request-error-code-of condition)))))

(define-test test-event-get-shouts
  (let ((response
         (cl-lastfm:event-get-shouts *api-key*
                                     "328799")))
    (assert-true (cl-ppcre:scan "<lfm status=\"ok\">" response))
    (assert-true (cl-ppcre:scan "<shouts event" response))))
