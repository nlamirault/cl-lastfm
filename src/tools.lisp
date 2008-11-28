;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          tools.lisp
;;;; Purpose:       Some tools for the cl-lastfm wrapper
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


(defun get-date-formated (control-string)
  "Get current date in CONTROL-STRING format."
 (multiple-value-bind
       (second minute hour date month year day-of-week dst-p tz)
     (decode-universal-time (get-universal-time) 0) ;;(get-decoded-time)
   (declare (ignore day-of-week dst-p tz))
   (format nil control-string
           year month date hour minute second)))


(defun get-current-date ()
  "Get current date in YYYY-MM-DD hh:mm:ss format."
  (get-date-formated "~2,'0d-~2,'0d-~2,'0d ~2,'0d:~2,'0d:~2,'0d"))


(defun get-timestamp ()
  "Get current date in YYYYMMDDHHMMSS format."
  (get-date-formated "~A~2,'0d~2,'0d~2,'0d~2,'0d~2,'0d"))


(defun unix-timestamp ()
  "Get the Unix timestamp : seconds since Jan 01 1970."
  (- (get-universal-time) (encode-universal-time 0 0 1 1 1 1970)))


(defun url-encode-utf8 (sequence)
  "URL encode in UTF-8 the sequence."
  (url-rewrite:url-encode
    (trivial-utf-8:utf-8-bytes-to-string
     (trivial-utf-8:string-to-utf-8-bytes sequence))))


(defun extract-lastfm-xml-error (xml)
  "Extract a LastFM error from a `XML' message.
Returns an error code and an error message."
  (let ((doc (cxml:parse xml (stp:make-builder)))
        code
        message)
    (stp:do-recursively (a doc)
      (when (and (typep a 'stp:element)
                 (equal (stp:local-name a) "error"))
        (setf code (stp:attribute-value a "code")
              message (stp:string-value a))))
    (values code message)))


(defun perform-lastfm-query (query &key parameters (method :get))
  "Retreive informations from LastFM Web Service.
 `QUERY' is the HTTP request.
`PARAMETERS' is a list of assoc list which specify HTTP parameters.
`METHOD' is the HTTP method used to perform the HTTP call."
  (when *debug*
    (format t "LastFM query : ~A~%" query))
  (multiple-value-bind (body status-code headers uri stream must-close)
      (if (equal :get method)
          (drakma:http-request query)
          (drakma:http-request query
                               :method method
                               :parameters parameters))
    (declare (ignore headers uri stream must-close))
    (when *debug*
      (format t "HTTP code : ~A~%" status-code))
    (if (and status-code (= status-code 200))
        body
        (multiple-value-bind (code msg)
            (extract-lastfm-xml-error body)
          (if (and code msg)
              (error 'lastfm-request-error
                     :code code
                     :message msg)
              (error 'lastfm-error :message (format nil "~A" body)))))))
        

