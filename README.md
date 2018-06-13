
# Simulated Spotify for Fundamentals I 

This repo is a prototype of a dumb stopable mp3 player simulated via a
big-bang interface. It also shows how to send mp3 files from a universe
server to a client program, which uses the prototype mp3 player to render
the file and request a "judgment" from the listener. 

## Dependency 

The repo depends on Leif A.'s video package (unstable version). Install
with DrRacket or at the command line with `raco pkg install
video-unstable`. 

## Files and Directories 

- prototypes
  - we-must-deliver-this : this prototype teachpack plays 2 roles. First,
    it implements and exports the play-sound function students could
    write. Second, it implements a main function that play-sound runs 
    as a subprocess. Communication happens via STDIN/OUT. 

    Could we implement this with a thread and simplify communication with events?

  - students-can-write-this : this shows how we would write a server and
    students would write a client 

- short.mp3: 10s sample mp3
- long.mp3: 3min sample mp3
 


