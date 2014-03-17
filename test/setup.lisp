;;
;; setup unit tests
;;

(in-package #:cl-lastfm-test)

(defun getenv (name)
  #+CMU (let ((x (assoc name ext:*environment-list*
                        :test #'string=)))
          (if x
            (cdr x)
            nil))
  #+Allegro (sys:getenv name)
  #+CLISP (ext:getenv name)
  #+ECL (si:getenv name)
  #+SBCL (sb-posix:getenv name)
  #+LISPWORKS (lispworks:environment-variable name))

(defparameter *api-key* (getenv "CL_LASTFM_API_KEY"))
