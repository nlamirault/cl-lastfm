;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          conditions.lisp
;;;; Purpose:       cl-lastfm conditions.
;;;; Programmer:    Nicolas Lamirault <nicolas.lamirault@gmail.com>
;;;;
;;;; This file, part of cl-lastfm, is Copyright (c) 2008 by Nicolas Lamirault
;;;;
;;;; cl-lastfm users are granted the rights to distribute and use this software
;;;; as governed by the terms of the MIT License :
;;;; http://www.opensource.org/licenses/mit-license.php
;;;;
;;;; *************************************************************************


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

