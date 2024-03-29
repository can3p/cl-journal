# Livejournal.com / Dreamwidth.org client written in common lisp

## The project is not dead, it's feature complete :D

The general idea is that I don't wont to write my posts in any web interface,
vim is just fine for these purposes! Moreover, the best workflow would by
similar to the one we get with the help of static site generators - git repo
with some files in markdown format which will be published on push.

Any file structure can be used, client will pick up any markdown files it can find

At the moment only post creation is rudimentary supported.

## Features

* Create posts as simple markdown files
* Privacy, Tags, Music, Location and Mood fields are supported
* Links to files in the repo resolve to links to relevant posts in the blog
* Update/removal of posts is supported when relevant file is modified/removed
* Optional git pre-commit hook to publish posts

## Install (Linux)

Guide was written based on Ubuntu linux, however there is nothing particularly
specific there apart from tools being used to download dependencies.

First of all cl-journal needs to store passwords somewhere, let's use secret-tools
for this.

`sudo apt-get install libsecret-tools`

After this we need roswell, you can grab latest release from the [project page][roswell].

Important detail, roswell will put executables into `~/.roswell/bin` folder
and it should be in path for script to work correctly:

`echo 'PATH="~/.roswell/bin:$PATH"' >> ~/.bashrc`

**Note**: A couple of cl-journal deps are dead now and cannot be found in the quicklisp. Please
help yourselves and clone `can3p/rpc4cl` and `can3p/rfc3339-timestamp` to `~/common-lisp`
folder.

Once roswell is installed, getting cl-journal is just a matter of

1. Install dead dependencies

Some of the libraries were kicked out from quicklisp and/or also gone from the github,
install them into `~/common-lisp` folder:

```
cd ~/common-lisp
git clone git@github.com:can3p/cl-arrows.git  # just a fork of official repo just in case
git clone git@github.com:can3p/rpc4cl.git                                                                                                           │
git clone git@github.com:can3p/rfc3339-timestamp.git
```

After this step you should be able to install the script

`ros install can3p/cl-journal`

## Install (Mac OS)

```bash
$ brew tap can3p/cl-journal
$ brew install cl-journal
```
Create a new repo for your blog and proceed to next section. Client setup is done!

## Current way of work

- fire `cl-journal init` and follow instructions
- Write posts in the following format:
- Refer lj users from markup with little effort

```
title: Name of your post
privacy: friends

Some body with *markdown* support, link to {lj-user livejournal}
```

- Privacy field is public by default. If specified can contain private, friends and public vlues
- Publish with `cl-journal push`

If you want to keep file in drafts for a while you can either
keep rejecting to post it during the sync or add a `draft: ` line
at the header of the file. In the latter case client will ignore
the file until you remove it.

## Running tests

Launch your favourite lisp REPL and:

~~~lisp
> (ql:quickload :cl-journal-test)
> (ql:quickload :prove)
> (setf prove:*enable-colors* nil) ;; disable colors if you do it from slime
> (prove:run :cl-journal-test)
~~~

## TODO or not supported yet

This list is not structured in a particular order

* reposts
* preview
* backwards sync to get updates from the service
* photo upload functionality

## License

All the code is in public domain, pull requests are welcome. Enjoy!

[roswell]: https://github.com/roswell/roswell/releases
