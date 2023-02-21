#lang pollen

<?xml version="1.0" encoding="utf-8"?>
<feed xmlns="http://www.w3.org/2005/Atom">
  <title>余日摇滚</title>
  <link href="◊(path->string (build-path root-url "rock/day"))" rel="self" />
  <updated>◊(rfc3339 "<2021-06-17 Thu 00:07>")</updated>
  <author>
    <name>◊|author-zh|</name>
  </author>
  <icon>◊(path->string (build-path root-url "favicon.png"))</icon>
  <id>urn:uuid:be474c46-ec1a-11eb-83a4-07f1c058be7a</id>
  ◊(require "pollen.rkt" pollen/template)
  ◊(set-rss-mode #t)
  ◊(->html (rock-day-feed-entries (output-files)) #:splice? #t)
</feed>
