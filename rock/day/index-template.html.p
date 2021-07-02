<!DOCTYPE html>
<html lang="zh">
  <head>
    <title>◊(->html (select 'h1 doc))</title>
    ◊(->html (essential-html-meta "rock/day/style.css"))
  </head>
  <body>
    ◊(->html (header-line #:rss
                          (rel-path "rock/day/atom.xml" (here-path))))
    <main>
      ◊(doc->html doc)
    </main>
  </body>
</html>
