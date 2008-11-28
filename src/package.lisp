;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          package.lisp
;;;; Purpose:       Package file for cl-lastfm
;;;; Programmer:    Nicolas Lamirault <nicolas.lamirault@gmail.com>
;;;;
;;;; This file, part of cl-lastfm, is Copyright (c) 2009 by Nicolas Lamirault
;;;;
;;;; cl-lastfm users are granted the rights to distribute and use this software
;;;; as governed by the terms of the MIT License :
;;;; http://www.opensource.org/licenses/mit-license.php
;;;;
;;;; *************************************************************************


(defpackage :cl-lastfm
  (:use :common-lisp)
  (:export

          ;; Albums

           #:album-get-info
           #:album-search

           ;; Artists

           #:artist-get-events
           #:artist-get-info
           #:artist-get-similar
           #:artist-get-top-albums
           #:artist-get-top-fans
           #:artist-get-top-tags
           #:artist-get-top-tracks
           #:artist-search


           ;; Geo

           #:geo-get-events
           #:geo-top-artists
           #:geo-top-tracks



           ;; User
           #:user-get-events
           #:user-get-friends
           #:user-get-loved-tracks
           #:user-get-neighbours

           ;; conditions

           #:lastfm-error
           #:error-message-of
           #:lastfm-request-error
           #:request-error-code-of
           #:lastfm-protocol-error


           ;; Debug

           #:*debug*

           )
  (:documentation "Common Lisp wrapper to Last.fm web service.
@begin[Albums]{section}
@aboutfun{album-get-info}
@aboutfun{album-search}
@end{section}

@begin[Artists]{section}
@aboutfun{artist-get-events}
@aboutfun{artist-get-info}
@aboutfun{artist-get-similar}
@aboutfun{artist-get-top-albums}
@aboutfun{artist-get-top-fans}
@aboutfun{artist-get-top-tags}
@aboutfun{artist-get-top-tracks}
@aboutfun{artist-search}
@end{section}

@begin[Geo]{section}
@aboutfun{geo-get-events}
@aboutfun{geo-top-artists}
@aboutfun{geo-top-tracks}
@end{section}

@begin[User]{section}
@aboutfun{user-get-events}
@aboutfun{user-get-friends}
@aboutfun{user-get-loved-tracks}
@aboutfun{user-get-neighbours}
@end{section}

"))




