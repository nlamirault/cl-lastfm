;;
;; Albums unit tests
;;

(in-package #:cl-lastfm-test)


(define-test test-album-get-info-without-api-key
  (handler-case
      (album-get-info "" :artist-name "U2" :album-name "War")
    (lastfm-request-error (condition)
      (assert-error 'lastfm-request-error condition)
      (assert-equal "10" (request-error-code-of condition)))))

(define-test test-album-get-info-without-artist
  (handler-case
      (cl-lastfm:album-get-info *api-key*
				:album-name "War")
    (lastfm-request-error (condition)
      (assert-error 'lastfm-request-error condition)
      (assert-equal "6" (request-error-code-of condition)))))

(define-test test-album-get-info
  (let ((response
         (cl-lastfm:album-get-info *api-key*
				   :artist-name "U2"
				   :album-name "War")))
    (assert-true (cl-ppcre:scan "<lfm status=\"ok\">" response))
    (assert-true (cl-ppcre:scan "<album>" response))
    (assert-true (cl-ppcre:scan "<name>War</name>" response))
    (assert-true (cl-ppcre:scan "<artist>U2</artist>" response))
    (assert-true (cl-ppcre:scan "<mbid>1be68d56-dbbd-3e4e-9fa8-98a71dc63ff3</mbid>"
				response))))

(define-test test-album-get-info-by-mbid
  (let ((response
         (cl-lastfm:album-get-info *api-key*
				   :mbid "1be68d56-dbbd-3e4e-9fa8-98a71dc63ff3")))
    (assert-true (cl-ppcre:scan "<lfm status=\"ok\">" response))
    (assert-true (cl-ppcre:scan "<album>" response))
    (assert-true (cl-ppcre:scan "<name>War</name>" response))
    (assert-true (cl-ppcre:scan "<artist>U2</artist>" response))
    (assert-true (cl-ppcre:scan "<mbid>1be68d56-dbbd-3e4e-9fa8-98a71dc63ff3</mbid>"
				response))))

(define-test test-search-album
  (let ((response
         (cl-lastfm:album-search *api-key* "Hello")))
    (assert-true (cl-ppcre:scan "<lfm status=\"ok\">" response))
    (assert-true (cl-ppcre:scan "<results for=\"Hello\" " response))
    (assert-true (cl-ppcre:scan "<name>Hello Nasty</name>" response))
    (assert-true (cl-ppcre:scan "<artist>Beastie Boys</artist>" response))))
