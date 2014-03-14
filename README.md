# cl-lastfm

[![Build Status](http://img.shields.io/travis/nlamirault/cl-lastfm.svg)](https://travis-ci.org/nlamirault/cl-lastfm)
[![Build Status](https://drone.io/github.com/nlamirault/cl-lastfm/status.png)](https://drone.io/github.com/nlamirault/cl-lastfm/latest)


A Common Lisp wrapper around [Last.fm](http://www.last.fm) 2.0 web service.

## Intallation

Add the projet and load it using [Quicklisp](http://www.quicklisp.org):

    CL-USER> (push #p"/projects/cl-lastfm/" asdf:*central-registry*)
    CL-USER> (ql:quickload "cl-lastfm")

## Hacking

Fork, hack and run unit tests:

    CL-USER> (setq lisp-unit:*print-failures* t)
	CL-USER> (setq lisp-unit:*print-errors* t)
	CL-USER> (setq lisp-unit:*print-summary* t)
	CL-USER> (setf cl-lastfm-test::*api-key* "xxxx")
	CL-USER> (lisp-unit:run-tests :all :cl-lastfm-test)

## Usage

Get a LastFM [API Key](http://www.lastfm.fr/api/accounts) and make some requests:

    CL-USER> (setq *api-key* "xxxxx")
    CL-USER> (cl-lastfm:artist-get-info *api-key* :artist-name "U2")

## API

The Last.fm Web Services API is described here :

    http://www.lastfm.fr/api

cl-lastfm implements theses features :

### Album

    [ ] album.addTags
	[x] album.getInfo
	[ ] album.getTags
	[ ] album.removeTag
	[x] album.search

### Artist

    [ ] artist.addTags
	[x] artist.getEvents
	[x] artist.getInfo
	[x] artist.getSimilar
	[ ] artist.getTags
	[x] artist.getTopAlbums
	[x] artist.getTopFans
	[x] artist.getTopTags
	[x] artist.getTopTracks
	[ ] artist.removeTag
	[x] artist.search
	[ ] artist.share

### Auth

	[ ] auth.getMobileSession
	[ ] auth.getSession
	[ ] auth.getToken
	[ ] auth.getWebSession

### Event

	[ ] event.attend
	[x] event.getInfo
	[ ] event.getShouts
	[ ] event.share

### Geo

	[x] geo.getEvents
	[x] geo.getTopArtists
	[x] geo.getTopTracks

### Group

	[x] group.getmembers
	[x] group.getWeeklyAlbumChart
	[x] group.getWeeklyArtistChart
	[x] group.getWeeklyChartList
	[x] group.getWeeklyTrackChart


### Library

	[ ] library.addAlbum
	[ ] library.addArtist
	[ ] library.addTrack
	[x] library.getAlbums
	[x] library.getArtists
	[x] library.getTracks

### Playlist

	[ ] playlist.addTrack
	[ ] playlist.create
	[ ] playlist.fetch

### Tag

	[x] tag.getSimilar
	[x] tag.getTopAlbums
	[x] tag.getTopArtists
	[x] tag.getTopTags
	[x] tag.getTopTracks
	[x] tag.getWeeklyArtistChart
	[x] tag.getWeeklyChartList
	[x] tag.search

### Tasteometer

	[x] tasteometer.compare

### Track

	[ ] track.addTags
	[ ] track.ban
	[x] track.getInfo
	[x] track.getSimilar
	[x] track.getTags
	[x] track.getTopFans
	[x] track.getTopTags
	[ ] track.love
	[ ] track.removeTag
	[x] track.search
	[ ] track.share

### User

	[x] user.getEvents
	[x] user.getFriends
	[ ] user.getInfo
	[x] user.getLovedTracks
	[x] user.getNeighbours
	[ ] user.getPastEvents
	[ ] user.getPlaylists
	[x] user.getRecentTracks
	[ ] user.getRecommendedEvents
	[x] user.getShouts
	[x] user.getTopAlbums
	[x] user.getTopArtists
	[x] user.getTopTags
	[x] user.getTopTracks
	[x] user.getWeeklyAlbumChart
	[x] user.getWeeklyArtistChart
	[x] user.getWeeklyChartList
	[x] user.getWeeklyTrackChart

## Changelog

A changelog is available [here](ChangeLog.md).


## Contact

Nicolas Lamirault <nicolas.lamirault@gmail.com>
