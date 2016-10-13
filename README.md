# Livejournal.com client written in common lisp

The general idea is that I don't wont to write my posts in any web interface,
vim is just fine for these purposes! Moreover, the best workflow would by
similar to the one we get with the help of static site generators - git repo
with some files in markdown format which will be published on push.

Any file structure can be used, client will pick up any markdown files it can find

At the moment only post creation is rudimentary supported.

Due to password storage only Mac OS X is supported at the moment.

## Features

* Create posts as simple markdown files
* Privacy, Tags, Music, Location and Mood fields are supported
* Links to files in the repo resolve to links to relevant posts in the blog
* Update/removal of posts is supported when relevant file is modified/removed

## Install

Install is a bit tricky, especially if you're not used to common lisp environment
and I'm not ready to provide any better way. Contributions are welcome!

- Install roswell (`brew install roswell` will do)
- Install sbcl (`ros install sbcl`)
- Clone this repo to ~/common-lisp/cl-journal folder `git clone https://github.com/can3p/cl-journal ~/common-lisp/cl-journal`
- add `~/.rosewell/bin` folder to your path (`echo 'PATH="$PATH:~/.roswell/bin"'`)
- add a symbolic link for cl-journal script (`ln -s ~/common-lisp/cl-journal/roswell/cl-journal.ros ~/.rosewell/bin/cl-journal.ros`)
- build it for better perforemance (`ros build ~/.rosewell/bin/cl-journal.ros`)
- Create a new repo for your blog and proceed to next section. Client setup is done!

## Current way of work

- setup local git repo for your posts and navigate there
- fire `cl-journal init` and follow instructions
- Write posts in the following format:
- Refer lj users from markup with little effort

```
title: Name of your post
privacy: friends

Some body with *markdown* support, link to {lj-user livejournal}
```
    
- Privacy field is public by default. If specified can contain private, friends and public vlues
- Publish with `cl-journal sync`

If you want to keep file in drafts for a while you can either
keep rejecting to post it during the sync or add a `draft: ` line
at the header of the file. In the latter case client will ignore
the file until you remove it.

## TODO or not supported yet

This list is not structured in a particular order

* reposts
* preview
* backwards sync to get updates from the livejournal.com
* photo upload functionality

## License

All the code is in public domain, pull requests are welcome. Enjoy!
