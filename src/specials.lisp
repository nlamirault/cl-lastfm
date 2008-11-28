;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          package.lisp
;;;; Purpose:       lastfm specials variables.
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


;; ------------------
;; HTTP Web Services
;; ------------------

(unless (boundp '+last-fm-ws+)
  (defconstant +last-fm-ws+ "http://ws.audioscrobbler.com/2.0/"))


;; ---------
;; API keys
;; ---------


(defparameter *api-key* nil)

(defparameter *api-secret-key* nil)


;; Artist

(unless (boundp '+artist-get-events+)
  (defconstant +artist-get-events+
    (concatenate 'string
                 +last-fm-ws+
                 "?method=artist.getevents&api_key=~A")))


(unless (boundp '+artist-get-info+)
  (defconstant +artist-get-info+
    (concatenate 'string
                 +last-fm-ws+
                 "?method=artist.getinfo&api_key=~A")))


(unless (boundp '+artist-get-similar+)
  (defconstant +artist-get-similar+
    (concatenate 'string
                 +last-fm-ws+
                 "?method=artist.getsimilar&api_key=~A")))


(unless (boundp '+artist-get-top-albums+)
  (defconstant +artist-get-top-albums+
    (concatenate 'string
                 +last-fm-ws+
                 "?method=artist.gettopalbums&api_key=~A")))


(unless (boundp '+artist-get-top-fans+)
  (defconstant +artist-get-top-fans+
    (concatenate 'string
                 +last-fm-ws+
                 "?method=artist.gettopfans&api_key=~A")))


(unless (boundp '+artist-get-top-tags+)
  (defconstant +artist-get-top-tags+
    (concatenate 'string
                 +last-fm-ws+
                 "?method=artist.gettoptags&api_key=~A")))


(unless (boundp '+artist-get-top-tracks+)
  (defconstant +artist-get-top-tracks+
    (concatenate 'string
                 +last-fm-ws+
                 "?method=artist.gettoptracks&api_key=~A")))


(unless (boundp '+artist-search+)
  (defconstant +artist-search+
    (concatenate 'string
                 +last-fm-ws+
                 "?method=artist.search&api_key=~A")))


;; Album

(unless (boundp '+album-get-info+)
  (defconstant +album-get-info+
    (concatenate 'string
                 +last-fm-ws+
                 "?method=album.getinfo&api_key=~A")))


(unless (boundp '+album-search+)
  (defconstant +album-search+
    (concatenate 'string
                 +last-fm-ws+
                 "?method=album.search&api_key=~A")))


;; Geo

(unless (boundp '+geo-get-events+)
  (defconstant +geo-get-events+
    (concatenate 'string
                 +last-fm-ws+
                 "?method=geo.getevents&api_key=~A")))


(unless (boundp '+geo-top-artists+)
  (defconstant +geo-top-artists+
    (concatenate 'string
                 +last-fm-ws+
                 "?method=geo.gettopartists&api_key=~A")))


(unless (boundp '+geo-top-tracks+)
  (defconstant +geo-top-tracks+
    (concatenate 'string
                 +last-fm-ws+
                 "?method=geo.gettoptracks&api_key=~A")))


;; User


(unless (boundp '+user-get-events)
  (defconstant +user-get-events
    (concatenate 'string
                 +last-fm-ws+
                 "?method=user.getevents&api_key=~A")))


(unless (boundp '+user-get-friends+)
  (defconstant +user-get-friends+
    (concatenate 'string
                 +last-fm-ws+
                 "?method=user.getfriends&api_key=~A")))


(unless (boundp '+user-get-loved-tracks+)
  (defconstant +user-get-loved-tracks+
    (concatenate 'string
                 +last-fm-ws+
                 "?method=user.getlovedtracks&api_key=~A")))


(unless (boundp '+user-get-neighbours+)
  (defconstant +user-get-neighbours+
    (concatenate 'string
                 +last-fm-ws+
                 "?method=user.getneighbours&api_key=~A")))



;; ---------------------
;; HTTP IO Protocol 1.1
;; ---------------------


;; (unless (boundp '+client-id+)
;;   (defconstant +client-id+ "tst"))

;; (unless (boundp '+client-version+)
;;   (defconstant +client-version+ "1.0"))


;; (unless (boundp '+protocol-identify-1.1+)
;;  (defconstant +protocol-identify-1.1+
;;    "http://post.audioscrobbler.com/?hs=true&p=1.1&c=~A&v=~A&u=~A"))


;; (unless (boundp '+protocol-post-1.1+)
;;  (defconstant +protocol-post-1.1+
;;    "u=~A&s=~A&a%5B0%5D=~A&t%5B0%5D=~A&b%5B0%5D=~A&m%5B0%5D=~A&l%5B0%5D=~A&i%5B0%5D=~A"))


;; (unless (boundp '+protocol-identify-1.2+)
;;  (defconstant +protocol-identify-1.2+
;;    "http://post.audioscrobbler.com/?hs=true&p=1.2&c=~A&v=~A&u=~A&t=~A&a=~A"))


;; (unless (boundp '+protocol-post-1.2+)
;;  (defconstant +protocol-post-1.2+
;;    "a%5b0%5d=~A&t%5b0%5d=~A&b%5b0%5d=~A&m%5b0%5d=~A&l%5b0%5d=~A&i%5b0%5d=~A&u=~A&"))
;; 

;; ------
;; Ouput
;; ------


(defparameter *debug* nil "If T activate some logs.")

