<!DOCTYPE html>
<html lang="◊(get-language "en")">
  <head>
    <title>◊(->html (select 'title doc))</title>
    ◊(->html (essential-html-meta "note/note-style.css"))
  </head>
  <body>
    ◊(->html (header-line #:rss (rel-path "note/atom.xml" (here-path))))

    <main id="body">
      ◊(doc->html (article-title doc))
      ◊;(doc->html (toc doc))
      ◊(doc->html doc)
    </main>
  </body>
</html>
