(in-package :cl-user)
(defpackage lj-parse-response.cl-journal-test-markdownify
  (:use :cl
        :prove)
  (:import-from :cl-journal.markdownify :markdownify)
  )
(in-package :lj-parse-response.cl-journal-test-markdownify)

(plan nil)

(subtest "simple paragraph"
  (is (markdownify "<p>I want to ahve it fetched back</p>")
      "I want to ahve it fetched back"))

(subtest "multiple paragraphs"
  (is (markdownify "<p>I want to ahve it fetched back</p><p>I want to ahve it fetched back</p>")
      "I want to ahve it fetched back

I want to ahve it fetched back"))

(subtest "paragraph and div"
  (is (markdownify "<p>I want to ahve it fetched back</p><div>I want to ahve it fetched back</div>")
      "I want to ahve it fetched back

<div>I want to ahve it fetched back</div>"))

(subtest "headers"
  (is (markdownify "<h1>Titles</h1><h2>Subtitles</h2><h3>Subsubtitlse</h3><h4>Subsubsubtitlse</h4>")
      "# Titles

## Subtitles

### Subsubtitlse

#### Subsubsubtitlse"))

(subtest "headers"
  (is (markdownify "<h1>Titles</h1><h2>Subtitles</h2><h3>Subsubtitlse</h3><h4>Subsubsubtitlse</h4>")
      "# Titles

## Subtitles

### Subsubtitlse

#### Subsubsubtitlse"))

(subtest "list"
  (is (markdownify "<ul><li>item 1</li><li>item 2</li><li>item 3</li></ul><p>rest</p>")
      "* item 1
* item 2
* item 3

rest"))

;; best effort there
(subtest "quote"
  (is (markdownify "<blockquote><ul><li>item 1</li><li>item 2</li><li>item 3</li></ul></blockquote><p>rest</p>")
      "> * item 1
> * item 2
> * item 3

rest"))

;; best effort here
(subtest "images and links"
  (is (markdownify "<p>this paragraph has a <a href=\"https://google.com\">link</a> </p><p><img src=\"https://octodex.github.com/images/yaktocat.png\" alt=\"Image of Yaktocat\"> </p>")
      "this paragraph has a [link](https://google.com) 

![Image of Yaktocat](https://octodex.github.com/images/yaktocat.png)"))

    "<strong>italic</strong> text <strong>bold</strong> text </p <code>inline</code> tags </p>"

;; best effort here
(subtest "misc"
  (is (markdownify    "<p><i>italic</i> text <strong>bold</strong> text <code>inline</code> tags </p>")
      "__italic__ text **bold** text `inline` tags"))

;; best effort here
(subtest "media"
  (is (markdownify    "<p><lj-embed id=\"9\" source=\"youtube\" vid=\"1bU7COLWJE0\" /></p><p><lj-embed id=\"10\" source=\"vimeo\" vid=\"268614303\" /></p>")
      "<iframe width=\"560\" height=\"315\" src=\"https://www.youtube.com/embed/1bU7COLWJE0\" frameborder=\"0\" allow=\"autoplay; encrypted-media\" allowfullscreen></iframe>

<iframe src=\"https://player.vimeo.com/video/268614303\" width=\"640\" height=\"360\" frameborder=\"0\" webkitallowfullscreen mozallowfullscreen allowfullscreen></iframe>"))

(finalize)
