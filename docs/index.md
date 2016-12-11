# Markdown based Livejournal.com client

cl-journal is a file-based command line client for Livejournal.com blogging
service. Every post is simply yet another markdown file with some fields
on the top.

## Why?

The general idea is that I don't wont to write my posts in any web interface,
since I have no need in a complex formatting - just basic styling and links.
With these requirements markdown is simply the best option and the best way to
write markdown is to do it in your favorite text editor.  Moreover, the best
workflow would by similar to the one we get with the help of static site
generators - git repo with some files in markdown format which will be
published on push.

Any file structure can be used, client will pick up any markdown files it can find.

## Features

* Create posts as simple markdown files
* Privacy, Tags, Music, Location and Mood fields are supported
* Links to files in the repo resolve to links to relevant posts in the blog
* Update/removal of posts is supported when relevant file is modified/removed

## Install

```bash
$ brew tap can3p/cl-journal
$ brew install cl-journal
```
Create a new repo for your blog and proceed to next section. Client setup is done!

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

## Frequently asked questions:

- *Why does client support only Max OS X?*

  I don't have any other operating system at hand, sorry. But pull requests are
  definitely welcome!

- *Which dialiect of markdown is supported?

  My belief is that I support all formatting documented on (official page)[https://daringfireball.net/projects/markdown/].
  Besides that you can refer to the posts you've already written with the client
  just by using their filenames and all these link will be converted to the links
  of relevant posts. Another addon is `{lj-user livejournal}` command that will
  generate a proper link to a lj user blog.

- *Why does client require git repository to be initialized?*

  Git is used to store journal username and initial idea was that you would like
  to backup your files somewhere and git is just the best option for the text. You
  can use a service like (bitbucket.org)[https://bitbucket.org] to create a private
  repository for free. Anyway this dependency will go away with next releases.

- *Where is my password stored?*

  cl-journal uses system keychain to store password, so it's as secure as keychain (I hope).

- *How can I help?*

  Pull requests are definitely welcome! If you see missing functionality, please send a patch
  or at least open an issue. If you see missing docs, don't hesitate to send a pull request
  to fix them as well.

## License

All the code is in public domain, pull requests are welcome. Enjoy!
