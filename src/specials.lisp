;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          package.lisp
;;;; Purpose:       lastfm specials variables.
;;;; Programmer:    Nicolas Lamirault <nicolas.lamirault@gmail.com>
;;;;
;;;; This file, part of cl-lastfm, is Copyright (c) 2008, 2009 by Nicolas Lamirault
;;;;
;;;; cl-lastfm users are granted the rights to distribute and use this software
;;;; as governed by the terms of the MIT License :
;;;; http://www.opensource.org/licenses/mit-license.php
;;;;
;;;; *************************************************************************


(in-package :cl-lastfm)



(eval-when (:compile-toplevel :load-toplevel :execute)

  (unless (boundp '+last-fm-ws+)
    (defconstant +last-fm-ws+ "http://ws.audioscrobbler.com/2.0/"
      "The LastFM web service."))

  (defmacro def-lastfm-uri (name uri)
    `(unless (boundp ',name)
       (defconstant ,name
         (concatenate 'string
                      +last-fm-ws+
                      ,uri)))))



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



(unless (boundp '+user-get-top-albums+)
  (defconstant +user-get-top-albums+
    (concatenate 'string
                 +last-fm-ws+
                 "?method=user.gettopalbums&api_key=~A")))


(unless (boundp '+user-get-top-artists+)
  (defconstant +user-get-top-artists+
    (concatenate 'string
                 +last-fm-ws+
                 "?method=user.gettopartists&api_key=~A")))


(unless (boundp '+user-get-top-tags+)
  (defconstant +user-get-top-tags+
    (concatenate 'string
                 +last-fm-ws+
                 "?method=user.gettoptags&api_key=~A")))


(unless (boundp '+user-get-top-tracks+)
  (defconstant +user-get-top-tracks+
    (concatenate 'string
                 +last-fm-ws+
                 "?method=user.gettoptracks&api_key=~A")))


(def-lastfm-uri +user-get-recent-tracks+
    "?method=user.getrecenttracks&api_key=~A")

(def-lastfm-uri +user-get-shouts+
    "?method=user.getshouts&api_key=~A")

(def-lastfm-uri +user-get-weekly-album-chart+
    "?method=user.getweeklyalbumchart&api_key=~A")

(def-lastfm-uri +user-get-weekly-artist-chart+
    "?method=user.getweeklyartistchart&api_key=~A")

(def-lastfm-uri +user-get-weekly-chart-list+
    "?method=user.getweeklychartlist&api_key=~A")

(def-lastfm-uri +user-get-weekly-track-chart+
    "?method=user.getweeklytrackchart&api_key=~A")





;; Group


(unless (boundp '+group-get-members+)
  (defconstant +group-get-members+
    (concatenate 'string
                 +last-fm-ws+
                 "?method=group.getmembers&api_key=~A")))


(unless (boundp '+group-get-weekly-album-chart+)
  (defconstant +group-get-weekly-album-chart+
    (concatenate 'string
                 +last-fm-ws+
                 "?method=group.getweeklyalbumchart&api_key=~A")))


(unless (boundp '+group-get-weekly-artist-chart+)
  (defconstant +group-get-weekly-artist-chart+
    (concatenate 'string
                 +last-fm-ws+
                 "?method=group.getweeklyartistchart&api_key=~A")))


(unless (boundp '+group-get-weekly-chart-list+)
  (defconstant +group-get-weekly-chart-list+
    (concatenate 'string
                 +last-fm-ws+
                 "?method=group.getweeklychartlist&api_key=~A")))


(unless (boundp '+group-get-weekly-track-chart+)
  (defconstant +group-get-weekly-track-chart+
    (concatenate 'string
                 +last-fm-ws+
                 "?method=group.getweeklytrackchart&api_key=~A")))




;; Tasteometer


(def-lastfm-uri +tasteometer-compare+
    "?method=tasteometer.compare&api_key=~A&type1=~A&type2=~A&value1=~A&value2=~A&limit=~A")





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

