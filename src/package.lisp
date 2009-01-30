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
           #:user-get-top-albums
           #:user-get-top-artists
           #:user-get-top-tracks
           #:user-get-top-tags
           #:user-get-recent-tracks
           #:user-get-shouts
           #:user-get-weekly-album-chart
           #:user-get-weekly-artist-chart
           #:user-get-weekly-chart-list
           #:user-get-weekly-track-chart


           ;; Group
           #:group-get-members 
           #:group-get-weekly-album-chart
           #:group-get-weekly-artist-chart
           #:group-get-weekly-chart-list
           #:group-get-weekly-track-chart

           
           ;; Events
           #:event-get-info
           #:event-get-shouts


           ;; Library
           #:library-get-albums
           #:library-get-artists
           #:library-get-tracks

           ;; Tag
           #:tag-get-similar
           #:tag-get-top-albums
           #:tag-get-top-artists
           #:tag-get-top-tracks
           #:tag-get-top-tags
           #:tag-get-weekly-artist-chart
           #:tag-get-weekly-chart-list
           #:tag-search

           ;; Tasteometer
           #:tasteometer-compare


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
See more explanation from the @a[http://www.lastfm.fr/api]{website}.
It has been tested under @a[http://www.sbcl.org]{SBCL}
 
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
@aboutfun{user-get-top-albums}
@aboutfun{user-get-top-artists}
@aboutfun{user-get-top-tags}
@aboutfun{user-get-top-tracks}
@aboutfun{user-get-recent-tracks}
@aboutfun{user-get-shouts}
@aboutfun{user-get-weekly-album-chart}
@aboutfun{user-get-weekly-artist-chart}
@aboutfun{user-get-weekly-chart-list}
@aboutfun{user-get-weekly-track-chart}
@end{section}

@begin[Group]{section}
@aboutfun{group-get-members}
@aboutfun{group-get-weekly-album-chart}
@aboutfun{group-get-weekly-artist-chart}
@aboutfun{group-get-weekly-chart-list}
@aboutfun{group-get-weekly-track-chart}
@end{section}

@begin[Library]{section}
@aboutfun{library-get-albums}
@aboutfun{library-get-artists}
@aboutfun{library-get-tracks}
@end{section}

@begin[Tag]{section}
@aboutfun{tag-get-similar}
@aboutfun{tag-get-similar}
@aboutfun{tag-get-top-albums}
@aboutfun{tag-get-top-artists}
@aboutfun{tag-get-top-tracks}
@aboutfun{tag-get-top-tags}
@aboutfun{tag-get-weekly-artist-chart}
@aboutfun{tag-get-weekly-chart-list}
@aboutfun{tag-search}
@end{section}

@begin[Events]{section}
@aboutfun{event-get-info}
@aboutfun{event-get-shouts}
@end{section}

@begin[TasteOMeter]{section}
@aboutfun{tasteometer-compare}
@end{section}

"))




