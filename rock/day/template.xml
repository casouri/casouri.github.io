<?xml version="1.0" encoding="utf-8"?>
<feed xmlns="http://www.w3.org/2005/Atom">
  <title>◊(select-from-metas 'title metas)</title>
  <link href="◊(path->string (build-path root-url (symbol->string here)))" rel="self" />
  <updated>◊(rfc3339 (select-from-metas 'updated metas))</updated>
  <author>
    <name>◊(select-from-metas 'author metas)</name>
  </author>
  <icon>◊(path->string (build-path root-url "favicon.png"))</icon>
  <id>urn:uuid:◊(select-from-metas 'uuid metas)</id>
  ◊(->html (rock-day-feed-entries (reverse (day-files))) #:splice? #t)
</feed>
