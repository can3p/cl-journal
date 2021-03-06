---
layout: home
title: Markdown based Livejournal.com / Dreamwidth.org client
---

cl-journal is a file-based command line client for Livejournal.com blogging
service and it's compatible clones like Dreamwidth.org. Every post is simply
yet another markdown file with some fields on the top.

WARNING: last version doesn't build with sbcl 1.4.3 due to some broken
dependencies. Sorry.

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
* Fetch posts from the server and store it in a row format and merge them into markdown
  including all supported fields!
* Privacy, Tags, Music, Location and Mood fields are supported
* Links to files in the repo resolve to links to relevant posts in the blog
* Update/removal of posts is supported when relevant file is modified/removed
* Quick look up of post urls by filename
* Optional git pre-commit hook to publish posts

## Install (Mac OS)

~~~bash
$ brew tap can3p/cl-journal
$ brew install cl-journal
~~~

## Install (Ubuntu linux)

1.  `sudo apt-get install libsecret-tools`
2. [Install][roswell] roswell
3.  `echo 'PATH="~/.roswell/bin:$PATH"' >> ~/.bashrc`
4. `ros install can3p/cl-journal`

~~~bash
$ brew tap can3p/cl-journal
$ brew install cl-journal
~~~

## Blog creation

~~~bash
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

Posting to groups is also supported in a very rudimentary way. What you
need to know about them is that this setting is sent to the api
as a number where every bit set flags that post should be visible to
a particular group. First bit is reserved for friends-only privacy mode,
so your custom groups will occupy bits from second onwards. You  may
set several bits if you want to make a post visible to several groups.
A particular group number can be derived from a list on the post creation
page, where first checkbox means second bit set, second checkbox - third
bit and so one. To make calculations simpler, let's assign a number
to every checkbox:

- Checkbox one - 2
- Checkbox two - 4
- Checkbox three - 8
- Checkbox four - 16
- Checkbox five - 32

To show this post to multiple groups you just need to sum their numbers up.
I.e. to make a post visible for checkbox one and three the value will be 10.
This is what you need to enter as privacy value:

~~~
privacy: 10
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
* photo upload functionality

## Frequently asked questions:

- *Where does the client work?*

  Client was tested and works on linux and Mac OS. If you're a happy
  user of any other operating system, feel free to make pull request
  with fixes for your operating system.

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

  On linux `secret-tools` library is used, so passwords are as secure as
  it is.

- *Does client support only livejournal.com service?*

  No! At the moment all livejournal clones should also work.
  Dreamwidth was tested and works and it's only one service which
  goes preconfigured along with livejournal. cl-journal can be
  used with any other livejournal clone, since endpoint can be
  configured during blog initialization.

  If you don't know what that means and want to have your service supported
  consider opening a [github issue](https://github.com/can3p/cl-journal/issues).

- *My old posts are unreadable, encoding is broken. WTF?*

  You probably did an effort to fix your encoding and went to
  [encoding](https://www.livejournal.com/settings/?c=OldEncoding)
  page and chose a correct one. Unfortunately livejournal messes
  this setting up and gives posts double encoded. Try to select
  no encoding on this page and fetch again to see if it helps. In
  this setting we try to download posts in different ways to
  get the content right.

- *I fetched and merged old posts, but in new release merge got improved. How
  can I generate post files again?*

  In case you didn't touch them afterwards you can always run `cl-journal remerge`
  and cl-journal will regenerate markdown files for all the posts. If merged
  post was updated and pushed later it will be left untouched.

- *How can I help?*

  Pull requests are definitely welcome, please check out [a source
  code](https://github.com/can3p/cl-journal)!  If you see missing
  functionality, please send a patch or at least open an issue. If you see
  missing docs, don't hesitate to send a pull request to fix them as well.

## License

All the code is in public domain, pull requests are welcome. Enjoy!

[roswell]: https://github.com/roswell/roswell/releases
