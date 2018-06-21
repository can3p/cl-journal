---
title: Changelog
---

## Not released yet

## 0.7.2

* 2018-06-22 Use getchallenge api call to get server timestamp on post updates

## 0.7.1

* 2018-06-19 Fix mark-as-pulled command to work as expected instead of infinite post fetching

## 0.7.0

* 2018-05-29 Remerge command to support free remerges for already merged posts
* 2018-05-24 Fetch functionality correctly handles 8bit encodings
* 2018-05-17 Merge functionality
* 2018-05-10 Fetch functionality stores sync timestamps for every item for later
  merging. Please remove source-posts.lisp file and run `cl-journal fetch` again.
* 2018-05-10 Every new post/update post will store a server timestamp along the post
  to make sync possible. Run `cl-journal fetch` and then `cl-journal mark-as-pulled`
  to get your db up to date. This command assumes that all local posts are the
  latest version available

## 0.6.0

* 2018-04-30 Fetch functionality! Technically you can get all your posts now

## 0.5.2

* 2018-02-22 Fix livejournal service initialization, which resulted in wrong service endpoint saved in config

## 0.5.1

* 2018-02-17 Support raw format, when markdown is sent to the server without being compiled to html
* 2018-02-17 Bug fix to initialize cl-journal correctly for custom services

## 0.5.0

* 2018-02-03 Linux support!
* 2018-02-03 `ignore-all` command. cl-journal relies on file timestamp to understand
  whether it was modified since the last sync. In case timestamp was
  modified but actual change did not happen, this command will help
  to calm cl-journal down and make him treat all posts as updated.

## 0.4.1

* 2018-01-02 Support filename to post url conversion in reference links

## 0.4.0

* 2017-06-10 Make code less livejournal specific, allow to store different passwords per service
* 2017-04-20 Initial dreamwidth support
* 2017-04-20 Fix api crash when badly structured props field is passed
* 2017-04-14 Switch from s-xml-rpc to rpc4cl to get https support

## 0.3.2

* 2017-04-05 `drafts` command is removed in favor of new `status` command that provides full information about local changes.

## 0.3.1

* fix typos that prevented `push` from working correctly

## 0.3.0

* 2016-12-26 Most basic new command ever
* 2016-12-26 Remove hard dependency on git, login is stored in db now
* 2016-12-17 `journal` field for posts to publish to specific communities

## 0.2.3

* 2016-12-17 `last` command to open system editor with last published file

## 0.2.2

* 2016-12-12 fix precommit hook code

## 0.2.1

* 2016-12-12 Misc fixes in help message and documentation

## 0.2.0

* 2016-12-12 sync command was renamed to push
* Github index page was set up

## 0.1.4

* 2016-11-28 Fix post link functionality
* 2016-11-27 Show program version in a help message

## 0.1.4

* 2016-11-26 Support drafts command
* 2016-11-06 Add Makefile to test buildapp binaries without brew
* 2016-11-06 Drop roswell support, since we can do everything with brew now

## 0.1.3

* 2016-11-05 Real fix for entry point

## 0.1.2

* 2016-11-05 Fix entry point
* 2016-11-04 Added info about installation via homebrew

## 0.1.1

* 2016-11-04 More fields in asd definition to please cl-brewer
* 2016-10-19 Move code out of roswell script so that we can reuse it for buildapp
* 2016-10-19 Massive refactoring to bring more sense into the code
* 2016-09-12 Update help
* 2016-09-12 Move package definitions into relevant files, use dot notation for naming
* 2016-09-11 Reorganize code according to cl-project structure

## 0.1.0

* 2016-08-30 [bugfix] Remove whitespace that was added to every line of post and broke markdown headers
* 2016-08-16 Posts updates and deletions
* 2016-08-14 Password input fixes
