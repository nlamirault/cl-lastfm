;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;; Copyright (C) 2014  Nicolas Lamirault

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
;; THE SOFTWARE.

(in-package :cl-lastfm)



(define-condition lastfm-error (simple-error)
  ((message :reader error-message-of
            :initarg :message
            :documentation "Explanation message."))
  (:documentation "Main Lastfm error.")
  (:report (lambda (condition stream)
             (format stream "Lastfm error : ~A."
                     (error-message-of condition)))))



(define-condition lastfm-request-error (lastfm-error)
  ((code :reader request-error-code-of
         :initarg :code
         :documentation "The error code."))
  (:documentation "Condition raised when an invalide request to the
Lastfm web services is performed.
Available codes are :
@pre{
    * 2 : Invalid service -This service does not exist
    * 3 : Invalid Method - No method with that name in this package
    * 4 : Authentication Failed - You do not have permissions to access the service
    * 5 : Invalid format - This service doesn't exist in that format
    * 6 : Invalid parameters - Your request is missing a required parameter
    * 7 : Invalid resource specified
    * 9 : Invalid session key - Please re-authenticate
    * 10 : Invalid API key - You must be granted a valid key by last.fm
    * 11 : Service Offline - This service is temporarily offline. Try again later.
    * 12 : Subscribers Only - This service is only available to paid last.fm subscribers
}
")
  (:report (lambda (condition stream)
             (format stream "Lastfm error ~A : ~A."
                     (request-error-code-of condition)
                     (error-message-of condition)))))


(define-condition lastfm-protocol-error (lastfm-error)
  ()
  (:documentation "Lastfm protocol error.")
  (:report (lambda (condition stream)
             (format stream "Lastfm Protocol error : ~A."
                     (error-message-of condition)))))
