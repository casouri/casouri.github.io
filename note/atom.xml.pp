#lang pollen

<?xml version="1.0" encoding="utf-8"?>
<feed xmlns="http://www.w3.org/2005/Atom">
  <title>Notes</title>
  <link href="◊(path->string (build-path root-url "note"))" rel="self" />
  <updated>◊(rfc3339)</updated>
  <author>
    <name>◊|author-en|</name>
  </author>
  <icon>◊(path->string (build-path root-url "favicon.png"))</icon>
  <id>urn:uuid:53fd03d4-ec1b-11eb-8cca-e7401fdbc2e2</id>
  ◊(require "pollen.rkt" pollen/template)
  ◊(set-rss-mode #t)
  ◊(->html (note-feed-entry "2023/tree-sitter-starter-guide"))
  ◊(->html (note-feed-entry "2023/tree-sitter-in-emacs-29"))
  ◊(->html (note-feed-entry "2022/domain-change"))
  ◊(->html (note-feed-entry "2022/nat-what-do-they-do"))
  ◊(->html (note-feed-entry "2021/fontset"))
  ◊(->html (note-feed-entry "2021/code-page-437"))
  ◊(->html (note-feed-entry "2021/dutch-801"))
  ◊(->html (note-feed-entry "2021/academica"))
  ◊(->html (note-feed-entry "2021/emacs-tree-sitter"))
  ◊(->html (note-feed-entry "2021/secure-pin-backing"))
  ◊(->html (note-feed-entry "2021/full-width-quote"))
</feed>
