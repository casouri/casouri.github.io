<!DOCTYPE html>
<html lang="zh">
  <head>
    <title>◊(->html (select 'h1 doc))</title>
    ◊(->html (essential-html-meta
              (or (select-from-metas 'stylesheet metas)
                  "rock/day/style.css")))
  </head>
  <body>
    ◊(->html (header-line #:rss "rock/day/atom.xml"))
    <main>
      ◊(doc->html doc)
    </main>
  </body>
</html>
