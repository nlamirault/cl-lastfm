cl-lastfm
=========

[![Build Status](http://img.shields.io/travis/nlamirault/cl-lastfm.svg)](https://travis-ci.org/nlamirault/cl-lastfm)


A Common Lisp wrapper around [Last.fm](http://www.last.fm) 2.0 web service.

API
===

The Last.fm Web Services API is described here :

    http://www.lastfm.fr/api

cl-lastfm implements theses features :

Album
------

    [ ] album.addTags
	[x] album.getInfo
	[ ] album.getTags
	[ ] album.removeTag
	[x] album.search

Artist
-------

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

Auth
----

	[ ] auth.getMobileSession
	[ ] auth.getSession
	[ ] auth.getToken
	[ ] auth.getWebSession

Event
------

	[ ] event.attend
	[x] event.getInfo
	[ ] event.getShouts
	[ ] event.share

Geo
---

	[x] geo.getEvents
	[x] geo.getTopArtists
	[x] geo.getTopTracks

Group
------

	[x] group.getmembers
	[x] group.getWeeklyAlbumChart
	[x] group.getWeeklyArtistChart
	[x] group.getWeeklyChartList
	[x] group.getWeeklyTrackChart


Library
--------

	[ ] library.addAlbum
	[ ] library.addArtist
	[ ] library.addTrack
	[x] library.getAlbums
	[x] library.getArtists
	[x] library.getTracks

Playlist
---------

	[ ] playlist.addTrack
	[ ] playlist.create
	[ ] playlist.fetch

Tag
----

	[x] tag.getSimilar
	[x] tag.getTopAlbums
	[x] tag.getTopArtists
	[x] tag.getTopTags
	[x] tag.getTopTracks
	[x] tag.getWeeklyArtistChart
	[x] tag.getWeeklyChartList
	[x] tag.search

Tasteometer
-------------

	[x] tasteometer.compare

Track
------

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

User
-----

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
