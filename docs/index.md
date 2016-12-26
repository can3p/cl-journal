# Markdown based Livejournal.com client

cl-journal is a file-based command line client for Livejournal.com blogging
service. Every post is simply yet another markdown file with some fields on the
top.

## Why?

The general idea is that I don't wont to write my posts in any web interface,
since I have no need in a complex formatting - just basic styling and links.
With these requirements markdown is simply the best option and the best way to
write markdown is to do it in your favorite text editor.  Moreover, the best
workflow would by similar to the one we get with the help of static site
generators - git repo with some files in markdown format which will be
published on push.

Another big advantage of this approach is that with local files it's much easier
to find a post you published some time ago - no need to go the your post pages,
just find the file with relevant post and link to it.

Any file structure can be used, client will pick up any markdown files it can
find.

## Features

* Create posts as simple markdown files
* Privacy, Tags, Music, Location and Mood fields are supported
* Links to files in the repo resolve to links to relevant posts in the blog
* Update/removal of posts is supported when relevant file is modified/removed
* Quick look up of post urls by filename
* Optional git pre-commit hook to publish posts

## Install

~~~bash
$ brew tap can3p/cl-journal
$ brew install cl-journal
$ mkdir yourblog.livejournal.com
$ cd yourblog.livejournal.com
$ cl-journal init
~~~

Client setup is done!

## Current way of work

To write a new blog post create a new markdown file in the folder with header
and actual post text separated be multiple newlines.  When ready fire
`cl-journal push` to get post published.

Here is an example of the post:

~~~
title: Name of your post
privacy: friends

Some body with *markdown* support, link to {lj-user livejournal}
~~~

Privacy field is public by default. If specified can contain private, friends
and public values.  The example has only title and privacy field but you can
use also tags, music, location and mood fields.

If you want to keep file in drafts for a while you can either keep rejecting to
post it during the sync or add a `draft: 1` line at the header of the file. In
the latter case client will ignore the file until you remove this line.

**Note** cl-journal also supports post updates and deletions. To do that just
edit/delete the relevant file and fire `cl-journal push` command from the
top level folder of your blog.

**Note** At the moment cl-journal does not sync any information from the
service, it just pushes them there. Hence if you update your post via
web interface client won't know anything about that.

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

  My belief is that I support all formatting documented on [official
  page](https://daringfireball.net/projects/markdown/).  Besides that you can
  refer to the posts you've already written with the client just by using their
  filenames and all these link will be converted to the links of relevant
  posts. Another addon is `{lj-user livejournal}` command that will generate a
  proper link to a lj user blog.

- *Where is my password stored?*

  cl-journal uses system keychain to store password, so it's as secure as
  keychain (I hope).

- *Why doesn't client support hostings like [dreamwidth](https://www.dreamwidth.org/)?*

  It doesn't for no other reason than because I don't use it myself! If you do
  consider opening a [github issue](https://github.com/can3p/cl-journal/issues).

- *How can I help?*

  Pull requests are definitely welcome, please check out [a source
  code](https://github.com/can3p/cl-journal)!  If you see missing
  functionality, please send a patch or at least open an issue. If you see
  missing docs, don't hesitate to send a pull request to fix them as well.

## License

All the code is in public domain, pull requests are welcome. Enjoy!
