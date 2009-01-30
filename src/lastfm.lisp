;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          lastfm.lisp
;;;; Purpose:       Common Lisp LastFM.
;;;; Programmer:    Nicolas Lamirault <nicolas.lamirault@gmail.com>
;;;;
;;;; This file, part of cl-lastfm, is Copyright (c) 2009 by Nicolas Lamirault
;;;;
;;;; cl-lastfm users are granted the rights to distribute and use this software
;;;; as governed by the terms of the MIT License :
;;;; http://www.opensource.org/licenses/mit-license.php
;;;;
;;;; *************************************************************************


(in-package :cl-lastfm)



;; Macro for LastFM requests.


(defmacro with-lastfm-stream (stream &body body)
  "Macro which creates an HTTP url, add parameters executing body,
and performs the HTTP request defined by uri."
  `(let (uri)
     (with-output-to-string (,stream)
       ,@body
       (setf uri (get-output-stream-string ,stream)))
    (perform-lastfm-query uri)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Albums
;;


(defun album-get-info (api-key &key artist-name album-name mbid lang)
  "@short{Get the metadata for an album on Last.fm using the album name or a
musicbrainz id. See @fun{playlist-fetch} on how to get the album playlist.
This service does not require authentication.}
@arg[api-key]{A Last.fm API key}
@arg[artist-name]{The artist name in question}
@arg[album-name]{The album name in question}
@arg[mbid]{The musicbrainz id for the album}
@arg[lang]{The language to return the biography in, expressed as an
ISO 639 alpha-2 code}
@see-condition{lastfm-request-error}
@return{An XML stream}"
  (let (uri)
    (with-output-to-string (stream)
      (format stream +album-get-info+ api-key)
      (when artist-name
        (format stream "&artist=~A" (url-encode-utf8 artist-name)))
      (when album-name
        (format stream "&album=~A" (url-encode-utf8 album-name)))
      (when mbid
        (format stream "&mbid=~A" (url-encode-utf8 mbid)))
      (when lang
        (format stream "&lang=~A" (url-encode-utf8 lang)))
      (setf uri (get-output-stream-string stream)))
    (perform-lastfm-query uri)))

;;;   (with-lastfm-uri (uri) stream
;;;     (format stream +album-get-info+ api-key)
;;;       (when artist-name
;;;         (format stream "&artist=~A" (url-encode-utf8 artist-name)))
;;;       (when album-name
;;;         (format stream "&album=~A" (url-encode-utf8 album-name)))
;;;       (when mbid
;;;         (format stream "&mbid=~A" (url-encode-utf8 mbid)))
;;;       (when lang
;;;         (format stream "&lang=~A" (url-encode-utf8 lang))))


    

(defun album-search (api-key album-name &key (limit 30) (page 1))
  "@short{Search for an album by name. Returns album matches sorted by relevance.
This service does not require authentication.}
@arg[api-key]{A Last.fm API key}
@arg[album-name]{The album name in question}
@arg[limit]{Limit the number of albums returned at one time. Default (maximum) 
is 30}
@arg[page]{Scan into the results by specifying a page number. Defaults to 
first page}
@see-condition{lastfm-request-error}
@return{An XML stream}"
  (let (uri)
    (with-output-to-string (stream)
      (format stream +album-search+ api-key)
      (when album-name
        (format stream "&album=~A" (url-encode-utf8 album-name)))
      (when limit
        (format stream "&limit=~A"
                (url-encode-utf8 (format nil "~A" limit))))
      (when page
        (format stream "&page=~A"
                (url-encode-utf8 (format nil "~A" page))))
      (setf uri (get-output-stream-string stream)))
    (perform-lastfm-query uri)))


    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Artists
;;


(defun artist-get-events (api-key artist-name)
  "@short{Get a list of upcoming events for this artist. Easily integratable
into calendars, using the ical standard. 
This service does not require authentication.}
@arg[api-key]{A Last.fm API key}
@arg[artist-name]{The artist name in question}
@see-condition{lastfm-request-error}
@return{An XML stream}"
  (let (uri)
    (with-output-to-string (stream)
      (format stream +artist-get-events+ api-key)
      (when artist-name
        (format stream "&artist=~A" (url-encode-utf8 artist-name)))
      (setf uri (get-output-stream-string stream)))
    (perform-lastfm-query uri)))


(defun artist-get-info (api-key &key artist-name mbid lang)
  "@short{Get the metadata for an artist on Last.fm. Includes biography. . 
This service does not require authentication.}
@arg[api-key]{A Last.fm API key}
@arg[artist-name]{The artist name in question}
@arg[mbid]{The musicbrainz id for the artist}
@arg[lang]{The language to return the biography in, expressed as an ISO 639
alpha-2 code}
@see-condition{lastfm-request-error}
@return{An XML stream}"
  (let (uri)
    (with-output-to-string (stream)
      (format stream +artist-get-info+ api-key)
      (when artist-name
        (format stream "&artist=~A" (url-encode-utf8 artist-name)))
      (when mbid
        (format stream "&mbid=~A" (url-encode-utf8 mbid)))
      (when lang
        (format stream "&lang=~A" (url-encode-utf8 lang)))
      (setf uri (get-output-stream-string stream)))
    (perform-lastfm-query uri)))


(defun artist-get-similar (api-key artist-name &optional limit)
  "@short{Get all the artists similar to this artist. 
This service does not require authentication.}
@arg[api-key]{A Last.fm API key}
@arg[artist-name]{The artist name in question}
@arg[limit]{Limit the number of similar artists returned}
@see-condition{lastfm-request-error}
@return{An XML stream}"
  (let (uri)
    (with-output-to-string (stream)
      (format stream +artist-get-similar+ api-key)
      (when artist-name
        (format stream "&artist=~A" (url-encode-utf8 artist-name)))
      (when limit
        (format stream "&limit=~A"
                (url-encode-utf8 (format nil "~A" limit))))
      (setf uri (get-output-stream-string stream)))
    (perform-lastfm-query uri)))




(defun artist-get-top-albums (api-key artist-name)
  "@short{Get the top albums for an artist on Last.fm, ordered by popularity. 
This service does not require authentication.}
@arg[api-key]{A Last.fm API key}
@arg[artist-name]{The artist name in question}
@see-condition{lastfm-request-error}
@return{An XML stream}"
  (let (uri)
    (with-output-to-string (stream)
      (format stream +artist-get-top-albums+ api-key)
      (when artist-name
        (format stream "&artist=~A" (url-encode-utf8 artist-name)))
      (setf uri (get-output-stream-string stream)))
    (perform-lastfm-query uri)))



(defun artist-get-top-fans (api-key artist-name)
  "@short{Get the top fans for an artist on Last.fm, based on listening data. 
This service does not require authentication.}
@arg[api-key]{A Last.fm API key}
@arg[artist-name]{The artist name in question}
@see-condition{lastfm-request-error}
@return{An XML stream}"
  (let (uri)
    (with-output-to-string (stream)
      (format stream +artist-get-top-fans+ api-key)
      (when artist-name
        (format stream "&artist=~A" (url-encode-utf8 artist-name)))
      (setf uri (get-output-stream-string stream)))
    (perform-lastfm-query uri)))



(defun artist-get-top-tags (api-key artist-name)
  "@short{Get the top tags for an artist on Last.fm, ordered by popularity. 
This service does not require authentication.}
@arg[api-key]{A Last.fm API key}
@arg[artist-name]{The artist name in question}
@see-condition{lastfm-request-error}
@return{An XML stream}"
  (let (uri)
    (with-output-to-string (stream)
      (format stream +artist-get-top-tags+ api-key)
      (when artist-name
        (format stream "&artist=~A" (url-encode-utf8 artist-name)))
      (setf uri (get-output-stream-string stream)))
    (perform-lastfm-query uri)))


(defun artist-get-top-tracks (api-key artist-name)
  "@short{Get the top tracks by an artist on Last.fm, ordered by popularity
This service does not require authentication.}
@arg[api-key]{A Last.fm API key}
@arg[artist-name]{The artist name in question}
@see-condition{lastfm-request-error}
@return{An XML stream}"
  (let (uri)
    (with-output-to-string (stream)
      (format stream +artist-get-top-tracks+ api-key)
      (when artist-name
        (format stream "&artist=~A" (url-encode-utf8 artist-name)))
      (setf uri (get-output-stream-string stream)))
    (perform-lastfm-query uri)))



(defun artist-search (api-key artist-name &key (limit 30) (page 1))
  "@short{Search for an artist by name. Returns artist matches sorted by
relevance. 
This service does not require authentication.}
@arg[api-key]{A Last.fm API key}
@arg[artist-name]{The artist name in question}
@arg[limit]{Limit the number of artists returned at one time. Default 
(maximum) is 30}
@arg[page]{Scan into the results by specifying a page number. Defaults to 
first page}
@see-condition{lastfm-request-error}
@return{An XML stream}"
  (let (uri)
    (with-output-to-string (stream)
      (format stream +artist-search+ api-key)
      (when artist-name
        (format stream "&artist=~A" (url-encode-utf8 artist-name)))
      (when limit
        (format stream "&limit=~A"
                (url-encode-utf8 (format nil "~A" limit))))
      (when page
        (format stream "&page=~A"
                (url-encode-utf8 (format nil "~A" page))))
      (setf uri (get-output-stream-string stream)))
    (perform-lastfm-query uri)))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Geo
;;



(defun geo-get-events (api-key &key location lat long page distance)
  "@short{Get all events in a specific location by country or city name.
This service does not require authentication}
@arg[api_key]{A Last.fm API key}
@arg[location]{Specifies a location to retrieve events for (service returns 
nearby events by default}
@arg[lat]{Specifies a latitude value to retrieve events for (service returns 
nearby events by default}
@arg[long]{Specifies a longitude value to retrieve events for (service returns 
nearby events by default)}
@arg[page]{Display more results by pagination}
@arg[distance]{Find events within a specified distance}
@see-condition{lastfm-request-error}
@return{An XML stream}"
  (let (uri)
    (with-output-to-string (stream)
      (format stream +geo-get-events+ api-key)
      (when location
        (format stream "&location=~A" (url-encode-utf8 location)))
      (when lat
        (format stream "&lat=~A" (url-encode-utf8 lat)))
      (when long
        (format stream "&long=~A" (url-encode-utf8 long)))
      (when distance
        (format stream "&distance=~A" (url-encode-utf8 distance)))
      (when page
        (format stream "&page=~A"
                (url-encode-utf8 (format nil "~A" page))))
      (setf uri (get-output-stream-string stream)))
    (perform-lastfm-query uri)))


(defun geo-top-artists (api-key country-name)
  "@short{Get the most popular artists on Last.fm by country.
This service does not require authentication}
@arg[api_key]{A Last.fm API key}
@arg[country-name]{A country name, as defined by the ISO 3166-1 country names 
standard}
@see-condition{lastfm-request-error}
@return{An XML stream}"
  (let (uri)
    (with-output-to-string (stream)
      (format stream +geo-top-artists+ api-key)
      (format stream "&country=~A" (url-encode-utf8 country-name))
      (setf uri (get-output-stream-string stream)))
    (perform-lastfm-query uri)))



(defun geo-top-tracks (api-key country-name &optional location)
  "@short{Get the most popular tracks on Last.fm by country .
This service does not require authentication}
@arg[api_key]{A Last.fm API key}
@arg[country-name]{A country name, as defined by the ISO 3166-1 country names 
standard}
@arg[location]{A metro name, to fetch the charts for (must be within the country 
specified)}
@see-condition{lastfm-request-error}
@return{An XML stream}"
  (let (uri)
    (with-output-to-string (stream)
      (format stream +geo-top-tracks+ api-key)
      (format stream "&country=~A" (url-encode-utf8 country-name))
      (when location
        (format stream "&location=~A" (url-encode-utf8 location)))
      (setf uri (get-output-stream-string stream)))
    (perform-lastfm-query uri)))







;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; User
;;



(defun user-get-events (api-key user)
  "@short{Get a list of upcoming events that this user is attending. 
Easily integratable into calendars, using the ical standard.
This service does not require authentication}
@arg[api_key]{A Last.fm API key}
@arg[user]{The user to fetch the events for}
@see-condition{lastfm-request-error}
@return{An XML stream}"
  (let (uri)
    (with-output-to-string (stream)
      (format stream +user-get-events api-key)
      (format stream "&user=~A" (url-encode-utf8 user))
      (setf uri (get-output-stream-string stream)))
    (perform-lastfm-query uri)))


(defun user-get-friends (api-key user &key limit recenttracks)
  "@short{Get a list of the user's friends on Last.fm
This service does not require authentication}
@arg[api_key]{A Last.fm API key}
@arg[user]{The last.fm username to fetch the friends of}
@arg[limit]{An integer used to limit the number of friends returned}
@arg[recenttracks]{Whether or not to include information about friends' recent 
listening in the response}
@see-condition{lastfm-request-error}
@return{An XML stream}"
  (let (uri)
    (with-output-to-string (stream)
      (format stream +user-get-friends+ api-key)
      (format stream "&user=~A" (url-encode-utf8 user))
      (when limit
        (format stream "&limit=~A"
                (url-encode-utf8 (format nil "~A" limit))))
      (when recenttracks
        (format stream "&recenttracks=~A"
                (url-encode-utf8 (format nil "~A" recenttracks))))
      (setf uri (get-output-stream-string stream)))
    (perform-lastfm-query uri)))



(defun user-get-loved-tracks (api-key user)
  "@short{Get the last 50 tracks loved by a user. 
This service does not require authentication}
@arg[api_key]{A Last.fm API key}
@arg[user]{The user name to fetch the loved tracks for}
@see-condition{lastfm-request-error}
@return{An XML stream}"
  (let (uri)
    (with-output-to-string (stream)
      (format stream +user-get-loved-tracks+ api-key)
      (format stream "&user=~A" (url-encode-utf8 user))
      (setf uri (get-output-stream-string stream)))
    (perform-lastfm-query uri)))


(defun user-get-neighbours (api-key user &key limit)
  "@short{Get a list of a user's neighbours on Last.fm
This service does not require authentication}
@arg[api_key]{A Last.fm API key}
@arg[user]{The last.fm username to fetch the neighbours of}
@arg[limit]{An integer used to limit the number of neighbours returned}
@see-condition{lastfm-request-error}
@return{An XML stream}"
  (let (uri)
    (with-output-to-string (stream)
      (format stream +user-get-neighbours+ api-key)
      (format stream "&user=~A" (url-encode-utf8 user))
      (when limit
        (format stream "&limit=~A"
                (url-encode-utf8 (format nil "~A" limit))))
      (setf uri (get-output-stream-string stream)))
    (perform-lastfm-query uri)))



(defun user-get-top-albums (api-key user &key period)
    "@short{Get the top albums listened to by a user. You can stipulate a time 
period. Sends the overall chart by default}
@arg[api_key]{A Last.fm API key}
@arg[user]{The user name to fetch top albums for}
@arg[period]{overall | 3month | 6month | 12month - The time period over which 
to retrieve top albums for.}
@see-condition{lastfm-request-error}
@return{An XML stream}"
  (let (uri)
    (with-output-to-string (stream)
      (format stream +user-get-top-albums+ api-key)
      (format stream "&user=~A" (url-encode-utf8 user))
      (when period
        (format stream "&period=~A"
                (url-encode-utf8 (format nil "~A" period))))
      (setf uri (get-output-stream-string stream)))
    (perform-lastfm-query uri)))



(defun user-get-top-artists (api-key user &key period)
    "@short{Get the top artists listened to by a user. You can stipulate a time 
period. Sends the overall chart by default.}
@arg[api_key]{A Last.fm API key}
@arg[user]{The user name to fetch top artists for}
@arg[period]{overall | 3month | 6month | 12month - The time period over which 
to retrieve top albums for.}
@see-condition{lastfm-request-error}
@return{An XML stream}"
  (let (uri)
    (with-output-to-string (stream)
      (format stream +user-get-top-artists+ api-key)
      (format stream "&user=~A" (url-encode-utf8 user))
      (when period
        (format stream "&period=~A"
                (url-encode-utf8 (format nil "~A" period))))
      (setf uri (get-output-stream-string stream)))
    (perform-lastfm-query uri)))


(defun user-get-top-tags (api-key user &key limit)
  "@short{Get the top tags used by this user}
@arg[api_key]{A Last.fm API key}
@arg[user]{The user name}
@arg[limit]{The number of tags returned}
@see-condition{lastfm-request-error}
@return{An XML stream}"
  (let (uri)
    (with-output-to-string (stream)
      (format stream +user-get-top-tags+ api-key)
      (format stream "&user=~A" (url-encode-utf8 user))
      (when limit
        (format stream "&limit=~A"
                (url-encode-utf8 (format nil "~A" limit))))
      (setf uri (get-output-stream-string stream)))
    (perform-lastfm-query uri)))


(defun user-get-top-tracks (api-key user &key period)
    "@short{Get the top tracks listened to by a user. You can stipulate a time 
period. Sends the overall chart by default. }
@arg[api_key]{A Last.fm API key}
@arg[user]{The user name to fetch top tracks for}
@arg[period]{overall | 3month | 6month | 12month - The time period over which 
to retrieve top tracks for.}
@see-condition{lastfm-request-error}
@return{An XML stream}"
  (let (uri)
    (with-output-to-string (stream)
      (format stream +user-get-top-tracks+ api-key)
      (format stream "&user=~A" (url-encode-utf8 user))
      (when period
        (format stream "&period=~A"
                (url-encode-utf8 (format nil "~A" period))))
      (setf uri (get-output-stream-string stream)))
    (perform-lastfm-query uri)))


(defun user-get-recent-tracks (api-key user &key limit)
  "@short{Get a list of the recent tracks listened to by this user. Indicates 
now playing track if the user is currently listening}
@arg[api_key]{A Last.fm API key}
@arg[user]{The user name to fetch the recent tracks of}
@arg[limit]{An integer used to limit the number of tracks returned}
@see-condition{lastfm-request-error}
@return{An XML stream}"
  (let (uri)
    (with-output-to-string (stream)
      (format stream +user-get-recent-tracks+ api-key)
      (format stream "&user=~A" (url-encode-utf8 user))
      (when limit
        (format stream "&limit=~A"
                (url-encode-utf8 (format nil "~A" limit))))
      (setf uri (get-output-stream-string stream)))
    (perform-lastfm-query uri)))


(defun user-get-shouts (api-key user)
  "@short{Get shouts for this user}
@arg[api_key]{A Last.fm API key}
@arg[user]{The user name to fetch shouts for}
@see-condition{lastfm-request-error}
@return{An XML stream}"
  (let (uri)
    (with-output-to-string (stream)
      (format stream +user-get-shouts+ api-key)
      (format stream "&user=~A" (url-encode-utf8 user))
      (setf uri (get-output-stream-string stream)))
    (perform-lastfm-query uri)))


(defun user-get-weekly-album-chart (api-key user &key from to)
  "@short{Get an album chart for a user profile, for a given date range. 
If no date range is supplied, it will return the most recent album chart for 
this user.}
@arg[api_key]{A Last.fm API key}
@arg[user]{The last.fm username to fetch the charts of}
@arg[from]{The date at which the chart should start from}
@arg[to]{The date at which the chart should end on}
@see-condition{lastfm-request-error}
@return{An XML stream}"
  (let (uri)
    (with-output-to-string (stream)
      (format stream +user-get-weekly-album-chart+ api-key)
      (format stream "&user=~A" (url-encode-utf8 user))
      (when from
        (format stream "&from=~A"
                (url-encode-utf8 (format nil "~A" from))))
      (when to
        (format stream "&to=~A"
                (url-encode-utf8 (format nil "~A" to))))
      (setf uri (get-output-stream-string stream)))
    (perform-lastfm-query uri)))


(defun user-get-weekly-artist-chart (api-key user &key from to)
  "@short{Get an artist chart for a user profile, for a given date range. 
If no date range is supplied, it will return the most recent artist chart for 
this user.}
@arg[api_key]{A Last.fm API key}
@arg[user]{The last.fm username to fetch the charts of}
@arg[from]{The date at which the chart should start from}
@arg[to]{The date at which the chart should end on}
@see-condition{lastfm-request-error}
@return{An XML stream}"
  (let (uri)
    (with-output-to-string (stream)
      (format stream +user-get-weekly-artist-chart+ api-key)
      (format stream "&user=~A" (url-encode-utf8 user))
      (when from
        (format stream "&from=~A"
                (url-encode-utf8 (format nil "~A" from))))
      (when to
        (format stream "&to=~A"
                (url-encode-utf8 (format nil "~A" to))))
      (setf uri (get-output-stream-string stream)))
    (perform-lastfm-query uri)))


(defun user-get-weekly-chart-list (api-key user)
  "@short{Get a list of available charts for this user, expressed as date 
ranges which can be sent to the chart services}
@arg[api_key]{A Last.fm API key}
@arg[user]{The last.fm username to fetch the charts list for}
@see-condition{lastfm-request-error}
@return{An XML stream}"
  (let (uri)
    (with-output-to-string (stream)
      (format stream +user-get-weekly-chart-list+ api-key)
      (format stream "&user=~A" (url-encode-utf8 user))
      (setf uri (get-output-stream-string stream)))
    (perform-lastfm-query uri)))


(defun user-get-weekly-track-chart (api-key user &key from to)
  "@short{Get a track chart for a user profile, for a given date range. 
If no date range is supplied, it will return the most recent track chart 
for this user}
@arg[api_key]{A Last.fm API key}
@arg[user]{The last.fm username to fetch the charts of}
@arg[from]{The date at which the chart should start from}
@arg[to]{The date at which the chart should end on}
@see-condition{lastfm-request-error}
@return{An XML stream}"
  (let (uri)
    (with-output-to-string (stream)
      (format stream +user-get-weekly-track-chart+ api-key)
      (format stream "&user=~A" (url-encode-utf8 user))
      (when from
        (format stream "&from=~A"
                (url-encode-utf8 (format nil "~A" from))))
      (when to
        (format stream "&to=~A"
                (url-encode-utf8 (format nil "~A" to))))
      (setf uri (get-output-stream-string stream)))
    (perform-lastfm-query uri)))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Groups
;;


(defun group-get-members (api-key group)
  "@short{Get a list of members for this group.
This service does not require authentication}
@arg[api_key]{A Last.fm API key}
@arg[group]{The group name to fetch the members of}
@see-condition{lastfm-request-error}
@return{An XML stream}"
  (let (uri)
    (with-output-to-string (stream)
      (format stream +group-get-members+ api-key)
      (format stream "&group=~A" (url-encode-utf8 group))
      (setf uri (get-output-stream-string stream)))
    (perform-lastfm-query uri)))
  


(defun group-get-weekly-album-chart (api-key group &key from to)
  "@short{Get an album chart for a group, for a given date range. If no date range 
is supplied, it will return the most recent album chart for this group.}
@arg[api_key]{A Last.fm API key}
@arg[group]{The last.fm group name to fetch the charts of}
@arg[from]{The date at which the chart should start from}
@arg[to]{The date at which the chart should end on}
@see-condition{lastfm-request-error}
@return{An XML stream}"
  (let (uri)
    (with-output-to-string (stream)
      (format stream +group-get-weekly-album-chart+ api-key)
      (format stream "&group=~A" (url-encode-utf8 group))
      (when from
        (format stream "&from=~A"
                (url-encode-utf8 (format nil "~A" from))))
      (when to
        (format stream "&to=~A"
                (url-encode-utf8 (format nil "~A" to))))
      (setf uri (get-output-stream-string stream)))
    (perform-lastfm-query uri)))


(defun group-get-weekly-artist-chart (api-key group &key from to)
  "@short{Get an artist chart for a group, for a given date range. If no date 
range is supplied, it will return the most recent album chart for this group}
@arg[api_key]{A Last.fm API key}
@arg[group]{The last.fm group name to fetch the charts of}
@arg[from]{The date at which the chart should start from}
@arg[to]{The date at which the chart should end on}
@see-condition{lastfm-request-error}
@return{An XML stream}"
  (let (uri)
    (with-output-to-string (stream)
      (format stream +group-get-weekly-artist-chart+ api-key)
      (format stream "&group=~A" (url-encode-utf8 group))
      (when from
        (format stream "&from=~A"
                (url-encode-utf8 (format nil "~A" from))))
      (when to
        (format stream "&to=~A"
                (url-encode-utf8 (format nil "~A" to))))
      (setf uri (get-output-stream-string stream)))
    (perform-lastfm-query uri)))

  
(defun group-get-weekly-chart-list (api-key group)
  "@short{Get a list of available charts for this group, expressed as date 
ranges which can be sent to the chart services}
@arg[api_key]{A Last.fm API key}
@arg[group]{The last.fm group name to fetch the charts list for}
@see-condition{lastfm-request-error}
@return{An XML stream}"
  (let (uri)
    (with-output-to-string (stream)
      (format stream +group-get-weekly-chart-list+ api-key)
      (format stream "&group=~A" (url-encode-utf8 group))
      (setf uri (get-output-stream-string stream)))
    (perform-lastfm-query uri)))


(defun group-get-weekly-track-chart (api-key group &key from to)
  "@short{Get a track chart for a group, for a given date range. If no date 
range is supplied, it will return the most recent album chart for this group}
@arg[api_key]{A Last.fm API key}
@arg[group]{The last.fm group name to fetch the charts of}
@arg[from]{The date at which the chart should start from}
@arg[to]{The date at which the chart should end on}
@see-condition{lastfm-request-error}
@return{An XML stream}"
  (let (uri)
    (with-output-to-string (stream)
      (format stream +group-get-weekly-track-chart+ api-key)
      (format stream "&group=~A" (url-encode-utf8 group))
      (when from
        (format stream "&from=~A"
                (url-encode-utf8 (format nil "~A" from))))
      (when to
        (format stream "&to=~A"
                (url-encode-utf8 (format nil "~A" to))))
      (setf uri (get-output-stream-string stream)))
    (perform-lastfm-query uri)))



;;; Tasteometer 


(defun tasteometer-compare (api-key first-type second-type first-value second-value
                            &key (limit 5))
  "@short{Get a Tasteometer score from two inputs, along with a list of 
shared artists. If the input is a User or a Myspace URL, some additional 
information is returned.}
@arg[api_key]{A Last.fm API key}
@arg[fisrt-type]{Must be user, artists or myspace}
@arg[second-type]{Must be user, artists or myspace}
@arg[first-value]{Must be : Last.fm username or comma-separated artist names 
or a MySpace profile URL}
@arg[second-value]{Must be : Last.fm username or comma-separated artist names 
or a MySpace profile URL}
@arg[limit]{How many shared artists to display}
@see-condition{lastfm-request-error}
@return{An XML stream}"
  (with-lastfm-stream stream
    (format stream +tasteometer-compare+
            api-key
            first-type second-type first-value second-value limit)))
            
            
