;;
;; setup unit tests
;;

(in-package #:cl-lastfm-test)

(defparameter *api-key* (sb-posix:getenv "CL_LASTFM_API_KEY"))
