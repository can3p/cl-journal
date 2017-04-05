---
title: Changelog
---

## Not released yet

* Nothing

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
