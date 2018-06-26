
# Simulated Spotify for Fundamentals I 

This repo is a prototype of a dumb stopable mp3 player simulated via a
big-bang interface. It also shows how to send mp3 files from a universe
server to a client program, which uses the prototype mp3 player to render
the file and request a "judgment" from the listener. 

## Dependency 

The repo depends on Leif A.'s video package (unstable version). Install
with DrRacket or at the command line with 
```
raco pkg install video-unstable
```

## Files and Directories 

- mp3-tags.rkt : a (brittle) approach to extracting basic tags from MP3 files

- prototypes
  - we-must-deliver-this : provide a function for playing/pausing an mp3
    byte string and delivering a string as a response 

  - our-server : this is a primitive version of our mp3 server 

  - students-can-write-this : this file shows how students would write an
    mp3 client that receives a song, plays it, and gets an opinion 


## Running Programs 

```
$ pwd
../mp3/
$ ./mp3-tags.rkt long.mp3
```
This delivers four pieces of information (title, artist, album, year/optional).

```
$ pwd 
..../prototypes/
$ racket our-server.rkt &
$ racket students-can-write-this.rkt 
```
This pops up a server window and two client windows per song. It plays the
following two. 

- short.mp3: 10s sample mp3
- long.mp3: 3min sample mp3
 


