# Livejournal.com client written in common lisp

The general idea is that I don't wont to write my posts in any web interface,
vim is just fine for these purposes! Moreover, the best workflow would by
similar to the one we get with the help of static site generators - git repo
with some files in markdown format which will be published on push.

At the moment only post creation is rudimentary supported.

## Current way of work

- setup local git repo for your posts and navigate there
- fire `cl-journal init` and follow instructions
- Write posts in the following format:

```
title: Name of your post

Some body with *markdown* support
```
    
- Publish with `cl-journal filename.md`
