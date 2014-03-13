;;
;; Users tests
;;

(in-package #:cl-lastfm-test)


(define-test cant-get-user-events-without-api-key
  (handler-case
      (user-get-events "" "rj")
    (cl-lastfm:lastfm-request-error (condition)
      (assert-error 'lastfm-request-error condition)
      (assert-equal "10" (request-error-code-of condition)))))


(define-test can-get-user-events-with-name
  (format t "API Key: ~A" (sb-posix:getenv "CL-LASTFM-API-KEY"))
  (let ((response (user-get-events *api-key* "rj")))
    (assert-true (cl-ppcre:scan "<lfm status=\"ok\">" response))
    (assert-true (cl-ppcre:scan "<events " response))
    (let ((sum 0))
      (cl-ppcre:do-matches (s e "<user>" response)
        (incf sum))
      (assert-true (>= sum 0)))))

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
