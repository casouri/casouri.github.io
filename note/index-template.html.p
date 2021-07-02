<!DOCTYPE html>
<html lang="◊(get-language "en")">
  <head>
    <title>◊(->html (select 'title doc))</title>
    ◊(->html (essential-html-meta "note/note-style.css"))
  </head>
  <body>
    ◊(->html (header-line #:rss (rel-path "note/atom.xml" (here-path))))

    <main id="body">
      ◊(->html (article-title doc))
      ◊(->html (post-proc doc) #:splice #t)
    </main>
  </body>
</html>
